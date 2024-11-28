use crate::file_spanned::FileSpanned;

use super::*;

#[derive(Debug, Clone)]
pub struct CallTargetIllegal {
  /// The name of the target.
  name: StrID,

  /// The location(s) of the illegal call
  locations: Vec<FileSpan>,

  /// The type of (non-function) thing that they tried to call. If there's
  /// `None`, then the target isn't defined at all.
  target_kind: Option<ItemKind>,
}
impl CallTargetIllegal {
  pub fn one_line(&self) -> String {
    use core::fmt::Write;
    let mut s = String::new();
    let name = self.name;
    if let Some(kind) = self.target_kind {
      write!(
        s,
        "Error: Call Target Illegal: `{name}` is a {kind}, not a Function: "
      )
      .ok();
    } else {
      write!(s, "Error: Call Target Illegal: `{name}` is not defined: ").ok();
    }
    for (i, file_span) in self.locations.iter().enumerate() {
      if i > 0 {
        write!(s, ", ").ok();
      }
      write!(s, "{file_span}").ok();
    }
    s
  }

  pub fn build_report(&self, config: Config) -> Report<FileSpan> {
    let file_span = self.locations.first().copied().unwrap();
    let name = self.name;
    if let Some(kind) = self.target_kind {
      Report::build(ReportKind::Error, file_span)
        .with_config(config)
        .with_message(format!(
          "Call Target Illegal: `{}` is a {kind}, not a Function.",
          self.name
        ))
        .with_labels(self.locations.iter().copied().map(Label::new))
        .finish()
    } else {
      Report::build(ReportKind::Error, file_span)
        .with_config(config)
        .with_message(format!(
          "Call Target Illegal: `{}` is not defined.",
          self.name
        ))
        .with_labels(self.locations.iter().copied().map(Label::new))
        .finish()
    }
  }
}

pub fn check_call_targets(
  items: &[FileSpanned<Item>], err_bucket: &mut Vec<YagError>,
) {
  let item_kinds: HashMap<StrID, ItemKind> = items
    .iter()
    .filter_map(|i| i.get_name().map(|name| (name._payload, i.kind())))
    .collect();
  let mut calls: HashMap<StrID, Vec<FileSpan>> = HashMap::new();
  for function in items.iter().flat_map(|i| i.get_function()) {
    for call_target in function.targets_called() {
      calls.entry(call_target._payload).or_default().push(call_target._span);
    }
  }
  for (target, sites) in calls {
    match item_kinds.get(&target) {
      Some(ItemKind::Function) => continue,
      other => {
        err_bucket.push(YagError::CallTargetIllegal(CallTargetIllegal {
          name: target,
          locations: sites,
          target_kind: other.copied(),
        }))
      }
    }
  }
}
