use super::*;

/// A "single chunk of code" in yagbas.
///
/// Most statements are "one line" of code, but the variants for control flow
/// have a body of inner statements that they cover.
///
/// * **Span Policy:** Span the heck out of this crap. The only time we don't
///   need a span is when we're storing a single-field variant that's a struct
///   which already has spans on its own fields.
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
  IfElse(IfElse),
  /// Load an 8-bit register with the given constant.
  LoadReg8Const {
    reg8: FileSpanned<Reg8>,
    expr: FileSpanned<ConstExpr>,
  },
  /// Store the `a` value to a constant address.
  ///
  /// ```yagbas
  /// [CONST_EXPR] = a
  /// ```
  StoreAToConstAddress(FileSpanned<ConstExpr>),
  /// Load a constant address into `a`.
  ///
  /// ```yagbas
  /// a = [CONST_EXPR]
  /// ```
  LoadAFromConstAddress(FileSpanned<ConstExpr>),
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
