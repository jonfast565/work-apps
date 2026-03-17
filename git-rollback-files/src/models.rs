#[derive(Debug, PartialEq, Clone)]
pub(crate) enum CompareAction {
    Insert(String),
    Delete(String),
    Update(String, String),
}

#[derive(Debug, PartialEq, Clone, clap::ValueEnum)]
pub(crate) enum CompareMode {
    ChooseSource,
    ChooseDestination
}