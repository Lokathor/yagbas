use super::*;

/// A "single chunk of code" in yagbas.
///
/// Most statements are "one line" of code, but the variants for control flow
/// have a body of inner statements that they cover.
#[derive(Debug, Clone)]
pub enum Statement {
  Return,
  Call {
    target: StrID,
    args: Vec<FileSpanned<TokenTree>>,
  },
  Loop(Loop),
  Continue(StrID),
  Break(StrID),
  /// Assign an 8-bit register the given constant value.
  AssignReg8Const {
    target: Reg8,
    value: FileSpanned<ConstExpr>,
  },
  StatementError,
}
impl Statement {
  #[inline]
  #[must_use]
  pub fn targets_called(&self) -> Vec<StrID> {
    match self {
      Statement::Call { target, .. } => vec![*target],
      Statement::Loop(Loop { statements, .. }) => {
        statements.iter().flat_map(|s| s.targets_called()).collect()
      }
      _ => Vec::new(),
    }
  }
}
