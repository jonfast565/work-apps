use std::path::{Path, PathBuf};

pub(crate) fn is_in_target_folder(path: &Path, target_folder: &Path) -> bool {
    let target_path = Path::new(target_folder);
    path.starts_with(target_path)
}

pub(crate) fn is_hidden_folder(path: &Path) -> bool {
    if let Some(first_component) = path.file_name() {
        if let Some(first_str) = first_component.to_str() {
            if first_str.starts_with('.') {
                return true;
            }
        }
    }
    false
}

pub(crate) fn is_special_folder(path: &Path) -> bool {
    // Check if the path is the .git folder
    if path.ends_with(".git") {
        return true;
    }

    // Check if the path is a subdirectory of .git
    if let Some(parent) = path.parent() {
        // Traverse up the directory tree
        for ancestor in parent.ancestors() {
            if ancestor.ends_with(".git") {
                return true;
            }
        }
    }

    false
}

pub(crate) fn normalize_path_with_forward_slashes<P: AsRef<Path>>(path: P) -> PathBuf {
    // Convert the original path to a string and replace backslashes with forward slashes
    let path_str = path.as_ref().display().to_string();
    let normalized_path_str = path_str.replace('\\', "/");

    // Return a PathBuf created from the normalized string
    PathBuf::from(normalized_path_str)
}

pub(crate) fn path_contains(parent: &PathBuf, child: &PathBuf) -> bool {
    child.ancestors().any(|ancestor| ancestor == parent)
}
