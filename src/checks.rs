use std::collections::{hash_map::Entry, HashMap, HashSet};

use crate::{
  errors::{MultipleDefinition, YagError},
  item::Item,
  src_files::{FileSpan, FileSpanned},
  str_id::StrID,
};

pub fn check_multiple_definitions(
  items: &[FileSpanned<Item>], err_bucket: &mut Vec<YagError>,
) {
  let mut definition_sites: HashMap<StrID, Vec<FileSpan>> = HashMap::new();
  for item in items {
    if let Some(name) = item.get_name() {
      match definition_sites.entry(name._payload) {
        Entry::Occupied(mut oe) => oe.get_mut().push(name._span),
        Entry::Vacant(ve) => drop(ve.insert(vec![name._span])),
      }
    }
  }
  for (name, sites) in definition_sites.into_iter() {
    if sites.len() > 1 {
      err_bucket.push(YagError::MultipleDefinition(MultipleDefinition {
        name,
        file_spans: sites,
      }))
    }
  }
}
