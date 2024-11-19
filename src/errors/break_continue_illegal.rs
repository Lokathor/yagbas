use super::*;

#[derive(Debug, Clone)]
pub struct BreakContinueIllegal {
  pub file_span: FileSpan,
  pub target: StrID,
}
impl BreakContinueIllegal {
  pub fn one_line(&self) -> String {
    todo!()
  }

  pub fn build_report(&self, config: Config) -> Report<FileSpan> {
    todo!()
  }
}

pub fn check_break_continue_illegal(
  items: &[FileSpanned<Item>], err_bucket: &mut Vec<YagError>,
) {
  for function in items.iter().flat_map(|i| i.get_function()) {
    todo!()
  }
}
