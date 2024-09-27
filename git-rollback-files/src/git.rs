use std::error::Error;
use std::fs;
use git2::{Repository, AnnotatedCommit, FetchOptions, RemoteCallbacks, Commit, ObjectType};
use std::path::{Path};
use crossbeam_channel::Receiver;
use walkdir::WalkDir;
use crate::files;

pub fn open_repository(target_folder: &Path) -> Result<Repository, git2::Error> {
    let repo = Repository::open(target_folder)?;
    Ok(repo)
}

pub fn init_repository(directory: &Path) -> Result<Repository, git2::Error> {
    Repository::init(directory)
}

pub fn clone_repository(url: &str, directory: &Path) -> Result<Repository, git2::Error> {
    Repository::clone(url, directory)
}

pub fn checkout_branch(repo: &Repository, branch_name: &str) -> Result<(), git2::Error> {
    let (object, reference) = repo.revparse_ext(branch_name)?;

    // checkout the tree
    repo.checkout_tree(&object, None)?;

    // set the pointer to the head
    if let Some(reference) = reference {
        repo.set_head(reference.name().unwrap())?;
    } else {
        repo.set_head_detached(object.id())?;
    }

    Ok(())
}

pub fn fetch_repository_from_origin(repo: &Repository) -> Result<(), git2::Error> {
    fetch_repository_from_remote(repo, "origin")
}

pub fn fetch_repository_from_remote(repo: &Repository, remote_name: &str) -> Result<(), git2::Error> {
    let mut remote = repo.find_remote(remote_name)?;

    // set up fetch options
    let callbacks = RemoteCallbacks::new();
    let mut fetch_options = FetchOptions::new();
    fetch_options.remote_callbacks(callbacks);

    // fetch updates from origin
    let ref_spec = format!("refs/heads/*:refs/remotes/{}/*", remote_name);
    remote.fetch(&[ref_spec], Some(&mut fetch_options), None)?;
    Ok(())
}

pub fn pull_branch(repo: &Repository) -> Result<(), git2::Error> {
    // Fetch new commits from origin
    fetch_repository_from_origin(repo)?;

    // Get the branch reference
    let fetch_head_ref = repo.find_reference("FETCH_HEAD")?;
    let fetch_head_commit = repo.reference_to_annotated_commit(&fetch_head_ref)?;

    // Merge the fetch head into the current branch
    merge_branch(repo, &fetch_head_commit)?;

    Ok(())
}

pub fn merge_branch(repo: &Repository, commit: &AnnotatedCommit) -> Result<(), git2::Error> {
    let mut index = repo.merge_commits(&repo.head()?.peel_to_commit()?, &repo.find_commit(commit.id())?, None)?;

    if index.has_conflicts() {
        println!("Conflicts detected during merge. Please resolve manually.");
        return Err(git2::Error::from_str("Merge conflicts occurred."));
    }

    // Write tree
    let oid = index.write_tree()?;
    let result_tree = repo.find_tree(oid)?;

    // Create a new commit to finalize the merge
    let sig = repo.signature()?;
    let head_commit = repo.head()?.peel_to_commit()?;
    repo.commit(Some("HEAD"), &sig, &sig, "Merge fetched changes", &result_tree, &[&head_commit, &repo.find_commit(commit.id())?])?;

    Ok(())
}

pub fn rollback_file_to_branch(repo_path: &Path, target_branch: &str, relative_file_path: &Path) -> Result<(), Box<dyn std::error::Error>> {
    let repo = open_repository(repo_path)?;

    let branch = repo.find_branch(target_branch, git2::BranchType::Local)?;
    let target_commit = branch.get().peel_to_commit()?;
    let target_tree = target_commit.tree()?;
    let tree_entry = match target_tree.get_path(relative_file_path) {
        Ok(entry) => entry,
        Err(_) => return Err(Box::new(git2::Error::from_str("File not found in target branch."))),
    };

    if tree_entry.kind() != Some(ObjectType::Blob) {
        return Err(Box::new(git2::Error::from_str("The specified path is not a file.")));
    }

    let blob_id = tree_entry.id();
    let blob = repo.find_blob(blob_id)?;

    let file_content = blob.content();
    let full_path = repo.workdir().unwrap().join(relative_file_path);

    fs::write(full_path, file_content)?;
    println!("File '{}' has been rolled back to the version in branch '{}'.", relative_file_path.display(), target_branch);
    
    let mut index = repo.index()?;
    index.add_path(relative_file_path)?;
    index.write()?;

    Ok(())
}

pub fn rollback_files_to_branch(repo_path: &str, branch_name: &str, target_folder: &str, cancellation_channel: Receiver<()>) -> Result<(), Box<dyn Error>> {
    let repo_path_canonical = Path::new(repo_path).canonicalize()?;
    let target_folder_canonical = Path::new(target_folder).canonicalize()?;

    let paths = WalkDir::new(&repo_path_canonical);
    for walk_path in paths {
        let path_dir_entry = walk_path?;
        let path = path_dir_entry.path();
        if path.is_file() && !files::is_in_target_folder(&path, &target_folder_canonical)
            && !files::is_hidden_folder(&path)
            && !files::is_special_folder(&path) {
            let relative_path = path.strip_prefix(&repo_path_canonical)?;
            match rollback_file_to_branch(&repo_path_canonical, branch_name, &relative_path) {
                Ok(..) => (),
                Err(e) => println!("Error rolling back file to branch '{}': {}", relative_path.display(), e),
            }
            if cancellation_channel.try_recv().is_ok() {
                println!("Task cancelled!");
                break;
            }
        }
    }
    Ok(())
}

fn get_commit_string(commit: &Commit) -> String {
    let id = commit.id();
    let author = commit.author();
    let author_name = &author.name().expect("Can't get author name");
    let author_email = &author.email().expect("Can't get author email");
    let message = commit.message().expect("Can't get commit message");

    format!("{} ({} <{}>): {}", id, author_name, author_email, message)
}

fn remove_lockfile(repo_path: &Path) -> Result<(), std::io::Error> {
    let lockfile = repo_path.join(".git").join("index.lock");
    if lockfile.exists() {
        fs::remove_file(&lockfile)?;
        println!("Lockfile removed: {:?}", lockfile);
    }
    Ok(())
}