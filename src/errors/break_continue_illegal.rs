use crate::ast::Statement;

use super::*;

#[derive(Debug, Clone)]
pub struct BreakContinueIllegal {
  pub file_span: FileSpan,
  pub target: StrID,
}
impl BreakContinueIllegal {
  pub fn one_line(&self) -> String {
    let name = self.target;
    let span = self.file_span;
    if name == StrID::from("") {
      format!("Error: Break/Continue Illegal: {span}: no unnamed outer loop.",)
    } else {
      format!(
        "Error: Break/Continue Illegal: {span}: no outer loop named `{name}`."
      )
    }
  }

  pub fn build_report(&self, config: Config) -> Report<FileSpan> {
    let name = self.target;
    let span = self.file_span;
    if name == StrID::from("") {
      Report::build(ReportKind::Error, span)
        .with_config(config)
        .with_message("Break/Continue Illegal: no unnamed outer loop.")
        .with_label(Label::new(span))
        .finish()
    } else {
      Report::build(ReportKind::Error, span)
        .with_config(config)
        .with_message(format!(
          "Break/Continue Illegal: no outer loop named `{name}`."
        ))
        .with_label(Label::new(span))
        .finish()
    }
  }
}

pub fn check_break_continue_illegal(
  items: &[FileSpanned<Item>], err_bucket: &mut Vec<YagError>,
) {
  let mut loop_names = Vec::new();
  for function in items.iter().flat_map(|i| i.get_function()) {
    for statement in function.statements.iter() {
      check_break_continue_statement(&mut loop_names, statement, err_bucket);
    }
  }
}

pub fn check_break_continue_statement(
  loop_names: &mut Vec<StrID>, statement: &FileSpanned<Statement>,
  err_bucket: &mut Vec<YagError>,
) {
  match statement._payload {
    Statement::Loop(ref l) => {
      loop_names.push(l.name);
      for s in l.statements.iter() {
        check_break_continue_statement(loop_names, s, err_bucket);
      }
      loop_names.pop();
    }
    Statement::Break(target) | Statement::Continue(target) => {
      for &name in loop_names.iter().rev() {
        if name == target {
          return;
        }
      }
      err_bucket.push(YagError::BreakContinueIllegal(BreakContinueIllegal {
        file_span: statement._span,
        target,
      }));
    }
    _ => (),
  }
}
