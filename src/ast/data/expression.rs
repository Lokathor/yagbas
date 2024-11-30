use super::*;

#[derive(Debug, Clone)]
pub enum Expression {
  NumLit(FileSpanned<StrID>),
  Ident(FileSpanned<StrID>),
  Register(FileSpanned<Register>),
  /// `x = y`
  Assign(Box<FileSpanned<Self>>, Box<FileSpanned<Self>>),
  /// `x == y`
  Eq(Box<FileSpanned<Self>>, Box<FileSpanned<Self>>),
  ExpressionError,
}
