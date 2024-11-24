use super::*;

/// A "single chunk of code" in yagbas.
///
/// Most statements are "one line" of code, but the variants for control flow
/// have a body of inner statements that they cover.
///
/// * **Span Policy:** It's assumed that `Statement` values will be stored by
///   other stuff as `FileSpanned<Statement>`. This means that 1-field variants
///   don't store a filespan around that single field. All other variants should
///   filespan each of their fields.
#[derive(Debug, Clone)]
pub enum Statement {
  Return,
  Call {
    target: FileSpanned<StrID>,
    args: Vec<FileSpanned<TokenTree>>,
  },
  Loop(Loop),
  Continue(StrID),
  Break(StrID),
  /// Assign an 8-bit register the given constant value.
  AssignReg8Const {
    target: FileSpanned<Reg8>,
    value: FileSpanned<ConstExpr>,
  },
  StatementError,
}
impl Statement {
  #[inline]
  #[must_use]
  pub fn targets_called(&self) -> Vec<FileSpanned<StrID>> {
    match self {
      Statement::Call { target, .. } => vec![*target],
      Statement::Loop(Loop { statements, .. }) => {
        statements.iter().flat_map(|s| s.targets_called()).collect()
      }
      _ => Vec::new(),
    }
  }
}
