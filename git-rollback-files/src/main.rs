#![allow(dead_code)]

use std::error::Error;
use clap::{Parser};
use crate::cancellation::spawn_task_with_cancellation;

mod git;
mod files;
mod cli;
mod cancellation;
mod models;
mod compare;
mod algorithm;

fn main() -> Result<(), Box<dyn Error>> {
    spawn_task_with_cancellation(|cancellation_channel| {
        let args = cli::CliArgs::parse();
        let repo_path = args.repo_folder.as_ref();
        let source_branch = args.source_branch.as_ref();
        let target_branch = args.target_branch.as_ref();
        let excluded_folders = args.excluded_folder;
        algorithm::compare_and_merge_changes(repo_path, source_branch, target_branch, excluded_folders, cancellation_channel)?;
        println!("Rollback process completed. Changes staged but not committed.");
        Ok(())
    })?;
    Ok(())
}

