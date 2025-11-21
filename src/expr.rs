use super::*;

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct Expr {
  span: Span32,
  kind: Box<ExprKind>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub enum ExprKind {
  #[default]
  ExprError,

  NumLit(ExprNumLit),
  Ident(ExprIdent),
  Bool(bool),

  /// `[ ... , ... , ... ]`
  ///
  /// square brackets around sub-expressions that forms a series of elements.
  List(ExprList),

  /// `{ ... ; ... ; ... }`
  ///
  /// braces around a series of statements.
  Block(ExprBlock),

  Call(ExprCall),
  Macro(ExprMacro),
  StructLit(ExprStructLit),

  IfElse(ExprIfElse),
  Loop(ExprLoop),
  LoopTimes(ExprLoopTimes),
  Break(ExprBreak),
  Continue(ExprContinue),
  Return(ExprReturn),

  UnOp(ExprUnOp),
  BinOp(ExprBinOp),
}

#[test]
fn test_expr_size() {
  // note(lokathor): any change in size might be justified (and so we would
  // update this test), but we should still take note of it happening.
  assert_eq!(size_of::<Expr>(), size_of::<[usize; 2]>());
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExprNumLit {
  lit: StrID,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExprIdent {
  ident: StrID,
}

/// Unary operation expression.
///
/// The span of "this" op ends up covering the inner expression as well as the
/// operator token, while the span of "inner" will naturally be the span of only
/// the inner sub-expression.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExprUnOp {
  inner: Expr,
  kind: UnOpKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnOpKind {
  /// `*x`
  Deref,
  /// `-x`
  Neg,
  /// `&x`
  Ref,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExprBinOp {
  lhs: Expr,
  rhs: Expr,
  kind: BinOpKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinOpKind {
  Assign,
  Add,
  Sub,
  Mul,
  Div,
  Mod,
  ShiftLeft,
  ShiftRight,
  BitAnd,
  BitOr,
  BitXor,
  BoolAnd,
  BoolOr,
  Index,
  Dot,
  Eq,
  Ne,
  Lt,
  Le,
  Gt,
  Ge,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExprCall {
  target: StrID,
  target_span: Span32,
  args: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExprMacro {
  target: StrID,
  target_span: Span32,
  args: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExprStructLit {
  ty: StrID,
  ty_span: Span32,
  args: Vec<FieldAssign>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub enum FieldAssign {
  #[default]
  FieldAssignError,
  Ident(StrID, Span32),
  IdentEq(StrID, Span32, Expr, Span32),
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct ExprIfElse {
  condition: Expr,
  if_body: Vec<Statement>,
  else_body: Vec<Statement>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct ExprList {
  elements: Vec<Expr>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct ExprBlock {
  body: Vec<Statement>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct ExprLoop {
  name: Option<StrID>,
  body: Vec<Statement>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct ExprLoopTimes {
  name: Option<StrID>,
  times: StrID,
  times_span: Span32,
  body: Vec<Statement>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct ExprBreak {
  target: Option<StrID>,
  target_span: Span32,
  val: Option<Expr>,
  val_span: Span32,
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub struct ExprContinue {
  target: Option<StrID>,
  target_span: Span32,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct ExprReturn {
  val: Option<Expr>,
  val_span: Span32,
}
