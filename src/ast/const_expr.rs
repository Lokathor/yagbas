use super::*;

#[derive(Debug, Clone)]
pub enum ConstExpr {
  Literal(StrID),
  Ident(StrID),
  Add(Box<FileSpanned<Self>>, Box<FileSpanned<Self>>),
  Sub(Box<FileSpanned<Self>>, Box<FileSpanned<Self>>),
  Neg(Box<FileSpanned<Self>>),
  Mul(Box<FileSpanned<Self>>, Box<FileSpanned<Self>>),
  Div(Box<FileSpanned<Self>>, Box<FileSpanned<Self>>),
  ConstExprError,
}
