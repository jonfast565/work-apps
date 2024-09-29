use git2::{AnnotatedCommit, Commit, DescribeOptions, FetchOptions, Index, ObjectType, RemoteCallbacks, Repository, Tree, TreeWalkMode, TreeWalkResult};
use std::error::Error;
use std::fs;
use std::path::Path;

pub(crate) fn open_repository(directory: &Path) -> Result<Repository, Box<dyn Error>> {
    let repo = Repository::open(directory)?;
    Ok(repo)
}

pub(crate) fn init_repository(directory: &Path) -> Result<Repository, Box<dyn Error>> {
    match Repository::init(directory) {
        Ok(repo) => Ok(repo),
        Err(e) => Err(e.into())
    }
}

pub(crate) fn clone_repository(url: &str, directory: &Path) -> Result<Repository, Box<dyn Error>> {
    match Repository::clone(url, directory) {
        Ok(repo) => Ok(repo),
        Err(e) => Err(e.into())
    }
}

pub(crate) fn checkout_branch(repo: &Repository, branch_name: &str) -> Result<(), Box<dyn Error>> {
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

pub(crate) fn fetch_repository_from_origin(repo: &Repository) -> Result<(), Box<dyn Error>> {
    fetch_repository_from_remote(repo, "origin")
}

pub(crate) fn fetch_repository_from_remote(repo: &Repository, remote_name: &str) -> Result<(), Box<dyn Error>> {
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

pub(crate) fn pull_branch(repo: &Repository) -> Result<(), Box<dyn Error>> {
    // Fetch new commits from origin
    fetch_repository_from_origin(repo)?;

    // Get the branch reference
    let fetch_head_ref = repo.find_reference("FETCH_HEAD")?;
    let fetch_head_commit = repo.reference_to_annotated_commit(&fetch_head_ref)?;

    // Merge the fetch head into the current branch
    merge_branch(repo, &fetch_head_commit)?;

    Ok(())
}

pub(crate) fn merge_branch(repo: &Repository, commit: &AnnotatedCommit) -> Result<(), Box<dyn Error>> {
    let mut index = repo.merge_commits(&repo.head()?.peel_to_commit()?, &repo.find_commit(commit.id())?, None)?;

    if index.has_conflicts() {
        println!("Conflicts detected during merge. Please resolve manually.");
        return Err(Box::from("Merge conflicts occurred."));
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

pub(crate) fn describe_repository(repo: &Repository) -> Result<(), Box<dyn Error>> {
    let describe_options = DescribeOptions::new();
    let describe = repo.describe(&describe_options)?;
    let describe_string = describe.format(None)?;
    println!("Repo Describe: {}", describe_string);
    Ok(())
}

pub(crate) fn rollback_file_to_branch(relative_file_path: &Path, repo: &Repository, tree: &Tree, index: &mut Index) -> Result<(), Box<dyn Error>> {
    let tree_entry = match tree.get_path(relative_file_path) {
        Ok(entry) => entry,
        Err(e) => {
            return Err(Box::new(e))
        }
    };

    if tree_entry.kind() != Some(ObjectType::Blob) {
        return Err(Box::from("The specified path is not a file."));
    }

    let blob_id = tree_entry.id();
    let blob = repo.find_blob(blob_id)?;

    let file_content = blob.content();
    let full_path = repo.workdir().unwrap().join(relative_file_path);

    fs::write(full_path, file_content)?;
    println!("File '{}' has been rolled back.", relative_file_path.display());

    index.add_path(relative_file_path)?;
    index.write()?;

    Ok(())
}

pub(crate) fn read_tree_file_paths(tree: &Tree) -> Result<Vec<String>, Box<dyn Error>> {
    let mut files = Vec::new();
    tree.walk(TreeWalkMode::PreOrder, |_, entry| {
        if let Some(name) = entry.name() {
            files.push(name.to_string());
        }
        TreeWalkResult::Ok
    })?;
    Ok(files)
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

