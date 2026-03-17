#![allow(dead_code)]

use std::{error::Error, path::{Path, PathBuf}};
use clap::Parser;
use crossbeam_channel::unbounded;
use crate::cancellation::spawn_task_with_cancellation;

mod git;
mod files;
mod cli;
mod cancellation;
mod models;
mod compare;
mod algorithm;

fn main() -> Result<(), Box<dyn Error>> {
    let (_, cancellation_channel) = unbounded();
    // let args = cli::CliArgs::parse();
    // let repo_path = args.repo_folder.as_ref();
    // let source_branch = args.source_branch.as_ref();
    // let target_branch = args.target_branch.as_ref();
    // let excluded_folders: Vec<PathBuf> = args.excluded_folder.iter().map(|x| Path::new(x).to_path_buf()).collect();
    // let compare_mode = args.compare_mode;
    let repo_path = Path::new("C:\\Repos\\Fire3");
    let source_branch = "core/power-aware-api-changes";
    let target_branch = "core/staging";
    let mut excluded_folders: Vec<PathBuf> = Vec::new();
    excluded_folders.push(Path::new("core/api/PowerAware.Api").to_path_buf());
    let compare_mode = models::CompareMode::ChooseDestination;
    algorithm::compare_and_merge_changes(repo_path, source_branch, target_branch, excluded_folders, compare_mode, cancellation_channel)?;
    println!("Rollback process completed. Changes staged but not committed.");
    spawn_task_with_cancellation(|cancellation_channel| {
        Ok(())
    })?;
    Ok(())
}

