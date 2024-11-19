use super::*;

#[derive(Debug, Clone)]
pub enum ConstExpr {
  Literal(StrID),
  Ident(StrID),
  Add(Box<Self>, Box<Self>),
  Sub(Box<Self>, Box<Self>),
  Neg(Box<Self>),
  Mul(Box<Self>, Box<Self>),
  Div(Box<Self>, Box<Self>),
  ConstExprError,
}
