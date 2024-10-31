use std::collections::{hash_map::Entry, HashMap, HashSet};

use crate::{item::Item, src_files::FileSpanned, str_id::StrID, YagError};

pub fn check_multiple_definitions(
  items: &[FileSpanned<Item>],
) -> Vec<YagError> {
  let mut definition_sites: HashMap<StrID, Vec<FileSpanned<StrID>>> =
    HashMap::new();
  for item in items {
    if let Some(name) = item.get_name() {
      match definition_sites.entry(name._payload) {
        Entry::Occupied(mut oe) => oe.get_mut().push(name),
        Entry::Vacant(ve) => drop(ve.insert(vec![name])),
      }
    }
  }
  let mut errors = Vec::new();
  for (name, sites) in definition_sites.into_iter() {
    if sites.len() > 1 {
      errors.push(YagError::MultipleDefinitions { name, sites })
    }
  }
  errors
}
