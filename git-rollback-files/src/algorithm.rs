use std::error::Error;
use std::path::{Path, PathBuf};
use crossbeam_channel::Receiver;
use git2::{BranchType, ObjectType, Repository, Tree};
use crate::compare::three_way_compare;
use crate::git;
use crate::git::{describe_repository, open_repository};
use crate::models::CompareAction;

pub(crate) struct RepoSuperBlock {
    repo_path: PathBuf,
    branch_name: String,
    repository: Repository
}

impl RepoSuperBlock {
    fn new(repo_path: &Path, branch_name: &str) -> Result<Self, Box<dyn Error>> {
        let repo_path_canonical = repo_path.canonicalize()?;
        let repo: Repository = open_repository(Path::new(&repo_path_canonical))?;
        Ok(Self {
            repo_path: repo_path.to_path_buf(),
            branch_name: branch_name.to_string(),
            repository: repo,
        })
    }
    fn read_tree_files(&self) -> Result<Vec<String>, Box<dyn Error>> {
        let branch_tree = self.get_branch_tree()?;
        git::read_tree_file_paths(&branch_tree)
    }
    fn read_file(&self, path: &Path) -> Result<Vec<u8>, Box<dyn Error>> {
        let branch_tree = self.get_branch_tree()?;
        let tree_entry = branch_tree.get_path(path)?;
        if tree_entry.kind() == Some(ObjectType::Blob) {
            let blob = self.repository.find_blob(tree_entry.id())?;
            Ok(blob.content().to_vec())
        } else {
            Err(Box::from("The specified path is not a file"))
        }
    }

    fn get_branch_tree(&self) -> Result<Tree, Box<dyn Error>> {
        let branch_object = self.repository.find_branch(self.branch_name.as_str(), BranchType::Local)?;
        let branch_commit = branch_object.get().peel_to_commit()?;
        let branch_tree = branch_commit.tree()?;
        Ok(branch_tree)
    }
}

pub(crate) fn compare_and_merge_changes(repo_path: &Path, source_branch: &str, target_branch: &str, excluded_paths: Vec<String>, cancellation_channel: Receiver<()>) -> Result<(), Box<dyn Error>> {
    let repo_path_canonical = Path::new(repo_path).canonicalize()?;
    let repo = open_repository(&repo_path_canonical)?;
    describe_repository(&repo)?;

    let source_repo_super_block = RepoSuperBlock::new(repo_path, &source_branch.to_string())?;
    let target_repo_super_block = RepoSuperBlock::new(repo_path, &target_branch.to_string())?;

    let source_files = source_repo_super_block.read_tree_files()?;
    let target_files = target_repo_super_block.read_tree_files()?;

    let source_files_excluded: Vec<_> = source_files
        .iter()
        .filter(|x| !excluded_paths.contains(x))
        .cloned()
        .collect();

    let target_files_excluded: Vec<_> = target_files
        .iter()
        .filter(|x| !excluded_paths.contains(x))
        .cloned()
        .collect();

    let compared = three_way_compare(source_files_excluded.as_slice(), target_files_excluded.as_slice());
    let inserts: Vec<_> = compared
        .iter()
        .filter(|action| matches!(action, CompareAction::Insert(_)))
        .collect();

    let updates: Vec<_> = compared
        .iter()
        .filter(|action| matches!(action, CompareAction::Update(_, _)))
        .collect();

    let deletes: Vec<_> = compared
        .iter()
        .filter(|action| matches!(action, CompareAction::Delete(_)))
        .collect();

    let mut index = (&repo).index()?;

    for insert_action in inserts {
        let CompareAction::Insert(item) = insert_action else { panic!("unexpected action") };
        index.add_path(item.as_ref())?;
        if cancellation_channel.try_recv().is_ok() {
            println!("Task was cancelled!");
            return Ok(());
        }
    }

    for update_action in updates {
        let CompareAction::Update(_source, target) = update_action else { panic!("unexpected action") };
        index.add_path(target.as_ref())?;
        if cancellation_channel.try_recv().is_ok() {
            println!("Task was cancelled!");
            return Ok(());
        }
    }

    for delete_action in deletes {
        let CompareAction::Delete(item) = delete_action else { panic!("unexpected action") };
        index.remove_path(item.as_ref())?;
        if cancellation_channel.try_recv().is_ok() {
            println!("Task was cancelled!");
            return Ok(());
        }
    }

    index.write()?;
    Ok(())
}