use super::*;

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct Expr {
  span: Span32,
  /// use `None` here instead of an explicit error kind.
  kind: Option<Box<ExprKind>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExprKind {
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

  Deref(ExprUnOp),
  Neg(ExprUnOp),
  Ref(ExprUnOp),

  Assign(ExprBinOp),
  Add(ExprBinOp),
  Sub(ExprBinOp),
  Mul(ExprBinOp),
  Div(ExprBinOp),
  Mod(ExprBinOp),
  ShiftLeft(ExprBinOp),
  ShiftRight(ExprBinOp),
  BitAnd(ExprBinOp),
  BitOr(ExprBinOp),
  BitXor(ExprBinOp),
  BoolAnd(ExprBinOp),
  BoolOr(ExprBinOp),
  Index(ExprBinOp),
  Dot(ExprBinOp),
  Eq(ExprBinOp),
  Ne(ExprBinOp),
  Lt(ExprBinOp),
  Le(ExprBinOp),
  Gt(ExprBinOp),
  Ge(ExprBinOp),
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExprUnOp {
  inner: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExprBinOp {
  lhs: Expr,
  rhs: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExprCall {
  target: StrID,
  target_span: Span32,
  args: Vec<ExprUnOp>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExprMacro {
  target: StrID,
  target_span: Span32,
  args: Vec<ExprUnOp>,
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
