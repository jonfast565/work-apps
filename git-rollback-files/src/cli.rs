use clap::Parser;
use crate::models::CompareMode;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
pub struct CliArgs {
    #[arg(short, long)]
    pub repo_folder: String,
    #[arg(short, long)]
    pub source_branch: String,
    #[arg(short, long)]
    pub target_branch: String,
    #[arg(short, long)]
    pub excluded_folder: Vec<String>,
    #[arg(short, long)]
    pub compare_mode: CompareMode
}