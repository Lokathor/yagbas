use super::*;

use token::Token;

#[derive(Debug, Clone)]
pub struct IfElse {
  pub test: FileSpanned<CompareTest>,
  pub if_body: Vec<FileSpanned<Statement>>,
  pub else_body: Vec<FileSpanned<Statement>>,
}

/// A test we can run using the `cp` instruction.
///
/// The `cp` instruction performs `a - x`, where `x` can be an imm8, a reg8, or
/// `[hl]`. The result is not stored anywhere, but the condition flags are set
/// according to the result.
///
/// * zero: `a` **equals** `x` when the zero flag is set, otherwise they aren't
///   equal.
/// * carry: `a` **is less than** `x` when the carry flag is set, otherwise it
///   is greater than or equal to `x`.
///
/// So we can easily test `a == x`, `a != x`, `a < x`, `a >= x`, but we
/// **cannot** test `a > x` cleanly.
#[derive(Debug, Clone)]
pub enum CompareTest {
  AEqualsImm(FileSpanned<ConstExpr>),
  ANotEqualsImm(FileSpanned<ConstExpr>),
  ALessThanImm(FileSpanned<ConstExpr>),
  AGreaterThanOrEqualImm(FileSpanned<ConstExpr>),
  //
  AEqualsReg8(FileSpanned<Reg8>),
  ANotEqualsReg8(FileSpanned<Reg8>),
  ALessThanReg8(FileSpanned<Reg8>),
  AGreaterThanOrEqualReg8(FileSpanned<Reg8>),
  //
  AEqualsHlTarget,
  ANotEqualsHlTarget,
  ALessThanHlTarget,
  AGreaterThanOrEqualHlTarget,
  //
  CompareTestError,
}
