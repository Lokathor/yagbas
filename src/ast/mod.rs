use std::collections::BTreeMap;

use super::*;

pub mod data;
pub mod parsing;

#[derive(Debug, Clone, Default)]
pub struct Ast {
  pub consts: BTreeMap<StrID, FileSpanned<Const>>,
  pub functions: BTreeMap<StrID, FileSpanned<Function>>,
  pub statics: BTreeMap<StrID, FileSpanned<Static>>,
}
impl Ast {
  pub fn from_items(
    items: Vec<FileSpanned<Item>>, err_bucket: &mut Vec<YagError>,
  ) -> Self {
    // Note(Lokathor): Currently all items are in a single namespace, so here we
    // verify that we don't define the same identifier more than once.
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
        match definition._payload {
          Item::Function(f) => assert!(out.functions.insert(name, f).is_none()),
          Item::Const(c) => assert!(out.consts.insert(name, c).is_none()),
          Item::Static(s) => assert!(out.statics.insert(name, s).is_none()),
          Item::ItemError => unimplemented!(),
        };
      }
    }
    out
  }
}
