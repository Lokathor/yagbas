use std::collections::BTreeMap;

use super::*;

pub mod data;
pub mod parsing;

#[derive(Debug, Clone, Default)]
pub struct Ast {
  pub evaluated_consts: BTreeMap<StrID, i32>,
  pub evaluated_statics: BTreeMap<StrID, Vec<i32>>,
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
      assert!(!definitions.is_empty());
      if definitions.len() > 1 {
        err_bucket.push(YagError::MultipleDefinitions(definitions))
      } else {
        let definition = definitions.pop().unwrap();
        let name = definition.get_name().unwrap();
        match definition._payload {
          Item::Function(f) => assert!(out.functions.insert(name, f).is_none()),
          Item::Const(c) => assert!(out.consts.insert(name, c).is_none()),
          Item::Static(s) => assert!(out.statics.insert(name, s).is_none()),
          Item::ItemError => unreachable!(),
        };
      }
    }
    out
  }

  pub fn run_const_eval(&mut self, err_bucket: &mut Vec<YagError>) {
    for (k, v) in self.consts.iter_mut() {
      let c = &mut v._payload;
      match c.expression._payload {
        Expression::NumLit(n) => {
          if let Some(x) = num_lit_to_i32(n) {
            self.evaluated_consts.insert(*k, x);
          } else {
            todo!("handle a num_lit that doesn't parse cleanly")
          }
        }
        _ => todo!("handle anything that isn't a num_lit"),
      }
    }
  }

  pub fn run_static_eval(&mut self, err_bucket: &mut Vec<YagError>) {
    for (k, v) in self.statics.iter_mut() {
      let s = &mut v._payload;
      let bytes = s
        .bytes
        .iter()
        .map(|b| match b._payload {
          Expression::NumLit(n) => {
            if let Some(x) = num_lit_to_i32(n) {
              x
            } else {
              todo!("handle a num_lit that doesn't parse cleanly")
            }
          }
          _ => todo!("handle anything that isn't a num_lit"),
        })
        .collect();
      self.evaluated_statics.insert(*k, bytes);
    }
  }
}

fn num_lit_to_i32(n: FileSpanned<StrID>) -> Option<i32> {
  // remove underscores
  let s: String = n.as_str().chars().filter(|c| *c != '_').collect();
  if let Some(b) = s.strip_prefix("%") {
    // binary
    i32::from_str_radix(b, 2).ok()
  } else if let Some(h) = s.strip_prefix("$") {
    // hexadecimal
    i32::from_str_radix(h, 16).ok()
  } else {
    // decimal
    i32::from_str_radix(&s, 10).ok()
  }
}
