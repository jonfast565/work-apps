use clap::Parser;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
pub struct CliArgs {
    #[arg(short, long)]
    pub repo_folder: String,
    #[arg(short, long)]
    pub target_folder: String,
    #[arg(short, long)]
    pub compare_branch: String
}