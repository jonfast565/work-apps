use crate::models::CompareAction;

pub fn three_way_compare(list1: &[String], list2: &[String]) -> Vec<CompareAction> {
    let mut actions = Vec::new();
    let max_len = std::cmp::max(list1.len(), list2.len());
    for i in 0..max_len {
        match (list1.get(i), list2.get(i)) {
            (Some(old), Some(new)) if old != new => {
                // Item exists in both lists but is different -> Update
                actions.push(CompareAction::Update(old.clone(), new.clone()));
            }
            (Some(old), None) => {
                // Item exists in the first list but not in the second -> Delete
                actions.push(CompareAction::Delete(old.clone()));
            }
            (None, Some(new)) => {
                // Item exists in the second list but not in the first -> Insert
                actions.push(CompareAction::Insert(new.clone()));
            }
            (Some(_), Some(_)) => {
                // No change, same item -> No action needed
            }
            _ => {}
        }
    }
    actions
}