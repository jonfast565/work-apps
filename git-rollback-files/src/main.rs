#![allow(dead_code)]

use std::error::Error;
use clap::{Parser};
use crate::cancellation::spawn_task_with_cancellation;

mod git;
mod files;
mod cli;
mod cancellation;

fn main() -> Result<(), Box<dyn Error>> {
    spawn_task_with_cancellation(|cancellation_channel| {
        let args = cli::CliArgs::parse();
        git::rollback_files_to_branch(args.repo_folder.as_str(), args.compare_branch.as_str(), args.target_folder.as_str())?;
        println!("Rollback process completed. Changes staged but not committed.");
        Ok(())
    })?;
    Ok(())
}