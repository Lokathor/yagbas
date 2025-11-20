use super::*;

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub enum Expr {
  #[default]
  ExprError,

  NumLit(Box<ExprNumLit>),
  Ident(Box<ExprIdent>),
  List(Box<Vec<ExprUnOp>>),
  Call(Box<ExprCall>),
  Macro(Box<ExprMacro>),
  Struct(Box<ExprStruct>),
  IfElse(Box<ExprIfElse>),
  Block(Box<ExprBlock>),

  Deref(Box<ExprUnOp>),
  Neg(Box<ExprUnOp>),
  Ref(Box<ExprUnOp>),

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
pub struct ExprStruct {
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
pub struct ExprBlock {
  body: Vec<Statement>,
  total_span: Span32,
}
