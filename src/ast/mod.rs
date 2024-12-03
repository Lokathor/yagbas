use std::collections::BTreeMap;

use super::*;

pub mod data;
pub mod parsing;

#[derive(Debug, Clone, Default)]
pub struct Ast {
  pub items: BTreeMap<StrID, FileSpanned<Item>>,
}
impl Ast {
  pub fn from_items(
    items: Vec<FileSpanned<Item>>, err_bucket: &mut Vec<YagError>,
  ) -> Self {
    let mut everything_map: BTreeMap<StrID, Vec<FileSpanned<Item>>> =
      BTreeMap::new();
    for item in items {
      if let Some(name) = item.get_name() {
        everything_map.entry(name).or_default().push(item);
      }
    }
    let mut out = Self::default();
    for mut definitions in everything_map.into_values() {
      if definitions.len() > 1 {
        err_bucket.push(YagError::MultipleDefinitions(definitions))
      } else {
        let definition = definitions.pop().unwrap();
        let name = definition.get_name().unwrap();
        assert!(out.items.insert(name, definition).is_none());
      }
    }
    out
  }
}
