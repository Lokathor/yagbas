use super::*;

#[derive(Debug, Clone)]
pub enum Expression {
  NumLit(FileSpanned<StrID>),
  Ident(FileSpanned<StrID>),
  Register(FileSpanned<Register>),
  Bool(FileSpanned<bool>),

  /// `[x]`
  Deref(Box<FileSpanned<Self>>),

  /// `x.y`
  Dot(Box<FileSpanned<Self>>, Box<FileSpanned<Self>>),

  /// `-x`
  Neg(Box<FileSpanned<Self>>),
  /// `&x`
  Ref(Box<FileSpanned<Self>>),

  /// `x * y`
  Mul(Box<FileSpanned<Self>>, Box<FileSpanned<Self>>),
  /// `x / y`
  Div(Box<FileSpanned<Self>>, Box<FileSpanned<Self>>),
  /// `x % y`
  Mod(Box<FileSpanned<Self>>, Box<FileSpanned<Self>>),

  /// `x + y`
  Add(Box<FileSpanned<Self>>, Box<FileSpanned<Self>>),
  /// `x - y`
  Sub(Box<FileSpanned<Self>>, Box<FileSpanned<Self>>),

  /// `x << y`
  ShiftLeft(Box<FileSpanned<Self>>, Box<FileSpanned<Self>>),
  /// `x >> y`
  ShiftRight(Box<FileSpanned<Self>>, Box<FileSpanned<Self>>),

  /// `x & y`
  BitAnd(Box<FileSpanned<Self>>, Box<FileSpanned<Self>>),

  /// `x ^ y`
  BitXor(Box<FileSpanned<Self>>, Box<FileSpanned<Self>>),

  /// `x | y`
  BitOr(Box<FileSpanned<Self>>, Box<FileSpanned<Self>>),

  /// `x == y`
  Eq(Box<FileSpanned<Self>>, Box<FileSpanned<Self>>),
  /// `x != y`
  Ne(Box<FileSpanned<Self>>, Box<FileSpanned<Self>>),
  /// `x < y`
  Lt(Box<FileSpanned<Self>>, Box<FileSpanned<Self>>),
  /// `x > y`
  Gt(Box<FileSpanned<Self>>, Box<FileSpanned<Self>>),
  /// `x <= y`
  Le(Box<FileSpanned<Self>>, Box<FileSpanned<Self>>),
  /// `x >= y`
  Ge(Box<FileSpanned<Self>>, Box<FileSpanned<Self>>),

  /// `x = y`
  Assign(Box<FileSpanned<Self>>, Box<FileSpanned<Self>>),

  ExpressionError,
}
