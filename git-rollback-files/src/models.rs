#[derive(Debug, PartialEq)]
pub(crate) enum CompareAction {
    Insert(String),
    Delete(String),
    Update(String, String),
}

#[derive(Debug, PartialEq)]
pub(crate) enum CompareMode {
    ChooseSource,
    ChooseDestination
}