use super::*;

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub enum Expr {
  #[default]
  ExprError,

  NumLit(Box<ExprNumLit>),
  Ident(Box<ExprIdent>),

  /// `[ ... , ... , ... ]`
  ///
  /// square brackets around sub-expressions that forms a series of elements.
  List(Box<ExprList>),
  /// `{ ... ; ... ; ... }`
  ///
  /// braces around a series of statements.
  Block(Box<ExprBlock>),

  Call(Box<ExprCall>),
  Macro(Box<ExprMacro>),
  StructLit(Box<ExprStructLit>),

  IfElse(Box<ExprIfElse>),
  Loop(Box<ExprLoop>),
  LoopTimes(Box<ExprLoopTimes>),
  Break(Box<ExprBreak>),
  Continue(Box<ExprContinue>),
  Return(Box<ExprReturn>),

  Deref(Box<ExprUnOp>),
  Neg(Box<ExprUnOp>),
  Ref(Box<ExprUnOp>),

  Assign(Box<ExprBinOp>),
  Add(Box<ExprBinOp>),
  Sub(Box<ExprBinOp>),
  Mul(Box<ExprBinOp>),
  Div(Box<ExprBinOp>),
  Mod(Box<ExprBinOp>),
  ShiftLeft(Box<ExprBinOp>),
  ShiftRight(Box<ExprBinOp>),
  BitAnd(Box<ExprBinOp>),
  BitOr(Box<ExprBinOp>),
  BitXor(Box<ExprBinOp>),
  BoolAnd(Box<ExprBinOp>),
  BoolOr(Box<ExprBinOp>),
  Index(Box<ExprBinOp>),
  Dot(Box<ExprBinOp>),
  Eq(Box<ExprBinOp>),
  Ne(Box<ExprBinOp>),
  Lt(Box<ExprBinOp>),
  Le(Box<ExprBinOp>),
  Gt(Box<ExprBinOp>),
  Ge(Box<ExprBinOp>),
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
  lit_span: Span32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExprIdent {
  ident: StrID,
  ident_span: Span32,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExprUnOp {
  inner: Expr,
  inner_span: Span32,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExprBinOp {
  lhs: Expr,
  lhs_span: Span32,
  rhs: Expr,
  rhs_span: Span32,
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
  total_span: Span32,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct ExprList {
  elements: Vec<Expr>,
  total_span: Span32,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct ExprBlock {
  body: Vec<Statement>,
  total_span: Span32,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct ExprLoop {
  name: Option<StrID>,
  body: Vec<Statement>,
  total_span: Span32,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct ExprLoopTimes {
  name: Option<StrID>,
  times: StrID,
  times_span: Span32,
  body: Vec<Statement>,
  total_span: Span32,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct ExprBreak {
  target: Option<StrID>,
  target_span: Span32,
  expr: Option<Expr>,
  expr_span: Span32,
  total_span: Span32,
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub struct ExprContinue {
  target: Option<StrID>,
  target_span: Span32,
  total_span: Span32,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct ExprReturn {
  expr: Option<Expr>,
  expr_span: Span32,
  total_span: Span32,
}
