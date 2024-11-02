use std::collections::{hash_map::Entry, HashMap};

use crate::item::Item;

use super::*;

#[derive(Debug, Clone)]
pub struct MultipleDefinition {
  pub name: StrID,
  pub file_spans: Vec<FileSpan>,
}
impl From<MultipleDefinition> for YagError {
  fn from(value: MultipleDefinition) -> Self {
    YagError::MultipleDefinition(value)
  }
}
impl MultipleDefinition {
  pub fn one_line(&self) -> String {
    use core::fmt::Write;
    let mut s = String::new();
    let name = self.name;
    write!(s, "Error: Multiple Definitions: `{name}`: ").ok();
    for (i, file_span) in self.file_spans.iter().enumerate() {
      if i > 0 {
        write!(s, ", ").ok();
      }
      write!(s, "{file_span}").ok();
    }
    s
  }

  pub fn build_report(&self, config: Config) -> Report<FileSpan> {
    let file_span = self.file_spans.first().copied().unwrap();
    Report::build(ReportKind::Error, file_span)
      .with_config(config)
      .with_message(format!("Multiple Definitions given for `{}`", self.name))
      .with_labels(self.file_spans.iter().copied().map(Label::new))
      .finish()
  }
}

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
