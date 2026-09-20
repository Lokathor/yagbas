//! Module full of operator info, for expression parsing.

/// Operator binding direction.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BindDirection {
  /// always binds left
  Left,
  /// always binds right
  Right,
  /// requires parentheses
  Ambiguious,
}

/// Operator that comes before the operand.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrefOp {
  /// `-x`
  Negative,
  /// `!x`
  BitNot,
  /// `*x`
  Dereference,
  /// `&x`
  Reference,
  /// `return x`
  Return,
  /// `break x`
  Break,
  /// `..x`, and `..`
  PrefixRangeExclusive,
  /// `..=x`, and `..=`
  PrefixRangeInclusive,
}
impl PrefOp {
  /// Gives the bind strength and direction for this operator.
  pub const fn binding(self) -> u8 {
    match self {
      Self::Return | Self::Break => 2,
      Self::PrefixRangeExclusive | Self::PrefixRangeInclusive => 6,
      Self::Negative | Self::BitNot | Self::Dereference | Self::Reference => 28,
    }
  }
  /// Some prefix operators don't need an operand.
  pub const fn needs_operand(self) -> bool {
    !matches!(
      self,
      Self::Return
        | Self::Break
        | Self::PrefixRangeExclusive
        | Self::PrefixRangeInclusive
    )
  }
  /// The token length is always 1.
  ///
  /// This is just here because infix operators are variable. Maybe it should
  /// get tossed out?
  pub const fn token_length(self) -> usize {
    1
  }
}

/// Operators that come after their operand.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PostOp {
  /// `x()`
  FnCall,
  /// `x[y]`
  ArrayIndex,
  /// `x?`
  Try,
  /// `x as y`
  As,
  /// `x..`
  PostfixRangeExclusive,
  /// `x..=`
  PostfixRangeInclusive,
}
impl PostOp {
  /// Gives the bind strength and direction for this operator.
  pub const fn binding(self) -> u8 {
    match self {
      Self::PostfixRangeExclusive | Self::PostfixRangeInclusive => 6,
      Self::As => 26,
      Self::Try => 30,
      Self::FnCall | Self::ArrayIndex => 32,
    }
  }

  /// The token length is always 1.
  ///
  /// This is just here because infix operators are variable. Maybe it should
  /// get tossed out?
  pub const fn token_length(&self) -> usize {
    1
  }
}

/// All the kinds of operator in Yagbas.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InfiOp {
  /// `x::y`
  Path,
  /// `x.y`
  Access,
  /// `x*y`
  Mul,
  /// `x/y`
  Div,
  /// `x%y`
  Rem,
  /// `x+y`
  Add,
  /// `x-y`
  Sub,
  /// `x<<y`
  ShiftLeft,
  /// `x>>y`
  ShiftRight,
  /// `x&y`
  BitAnd,
  /// `x^y`
  BitXor,
  /// `x|y`
  BitOr,
  /// `x==y`
  CmpEq,
  /// `x!=y`
  CmpNe,
  /// `x<y`
  CmpLt,
  /// `x>y`
  CmpGt,
  /// `x<=y`
  CmpLe,
  /// `x>=y`
  CmpGe,
  /// `x&&y`
  ConditionalAnd,
  /// `x||y`
  ConditionalOr,
  /// `x..y`
  RangeExclusive,
  /// `x..=y`
  RangeInclusive,
  /// `x=y`
  Assign,
  /// `x+=y`
  AddAssign,
  /// `x-=y`
  SubAssign,
  /// `x*=y`
  MulAssign,
  /// `x/=y`
  DivAssign,
  /// `x%=y`
  RemAssign,
  /// `x&=y`
  BitAndAssign,
  /// `x|=y`
  BitOrAssign,
  /// `x^=y`
  BitXorAssign,
  /// `x>>=y`
  ShiftLeftAssign,
  /// `x<<=y`
  ShiftRightAssign,
}
impl InfiOp {
  /// Gives the bind strength and direction for this operator.
  pub const fn binding(self) -> u8 {
    match self {
      Self::Assign
      | Self::AddAssign
      | Self::SubAssign
      | Self::MulAssign
      | Self::DivAssign
      | Self::RemAssign
      | Self::BitAndAssign
      | Self::BitOrAssign
      | Self::BitXorAssign
      | Self::ShiftLeftAssign
      | Self::ShiftRightAssign => 4,
      Self::RangeExclusive | Self::RangeInclusive => 6,
      Self::ConditionalOr => 8,
      Self::ConditionalAnd => 10,
      Self::CmpEq
      | Self::CmpNe
      | Self::CmpLt
      | Self::CmpGt
      | Self::CmpLe
      | Self::CmpGe => 12,
      Self::BitOr => 14,
      Self::BitXor => 16,
      Self::BitAnd => 18,
      Self::ShiftLeft | Self::ShiftRight => 20,
      Self::Add | Self::Sub => 22,
      Self::Mul | Self::Div | Self::Rem => 24,
      Self::Access => 34,
      Self::Path => 36,
    }
  }
  /// If this operator leans left, right, or is ambiguious and requires parens.
  pub const fn direction(self) -> BindDirection {
    match self {
      Self::ConditionalOr
      | Self::ConditionalAnd
      | Self::BitOr
      | Self::BitXor
      | Self::BitAnd
      | Self::ShiftLeft
      | Self::ShiftRight
      | Self::Add
      | Self::Sub
      | Self::Mul
      | Self::Div
      | Self::Rem
      | Self::Access
      | Self::Path => BindDirection::Left,
      Self::Assign
      | Self::AddAssign
      | Self::SubAssign
      | Self::MulAssign
      | Self::DivAssign
      | Self::RemAssign
      | Self::BitAndAssign
      | Self::BitOrAssign
      | Self::BitXorAssign
      | Self::ShiftLeftAssign
      | Self::ShiftRightAssign => BindDirection::Right,
      Self::RangeExclusive
      | Self::RangeInclusive
      | Self::CmpEq
      | Self::CmpNe
      | Self::CmpLt
      | Self::CmpGt
      | Self::CmpLe
      | Self::CmpGe => BindDirection::Ambiguious,
    }
  }

  /// Infix operators are NOT all a single token long.
  ///
  /// This tells you how many tokens the parser has to step forward to move over
  /// this particular operator.
  pub const fn token_length(&self) -> usize {
    match self {
      InfiOp::ShiftLeftAssign | InfiOp::ShiftRightAssign => 3,
      InfiOp::ShiftLeft
      | InfiOp::ShiftRight
      | InfiOp::ConditionalAnd
      | InfiOp::ConditionalOr
      | InfiOp::CmpGe
      | InfiOp::CmpLe => 2,
      _ => 1,
    }
  }
}

/// A binary operator
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum BinOpKind {
  /// error kind, mostly used as the default.
  #[default]
  ErrBinOpKind,
  /// `x[y]`
  ArrayIndex,
  /// `x::y`
  Path,
  /// `x.y`
  Access,
  /// `x*y`
  Mul,
  /// `x/y`
  Div,
  /// `x%y`
  Rem,
  /// `x+y`
  Add,
  /// `x-y`
  Sub,
  /// `x<<y`
  ShiftLeft,
  /// `x>>y`
  ShiftRight,
  /// `x&y`
  BitAnd,
  /// `x^y`
  BitXor,
  /// `x|y`
  BitOr,
  /// `x==y`
  CmpEq,
  /// `x!=y`
  CmpNe,
  /// `x<y`
  CmpLt,
  /// `x>y`
  CmpGt,
  /// `x<=y`
  CmpLe,
  /// `x>=y`
  CmpGe,
  /// `x&&y`
  ConditionalAnd,
  /// `x||y`
  ConditionalOr,
  /// `x..y`
  RangeExclusive,
  /// `x..=y`
  RangeInclusive,
  /// `x=y`
  Assign,
  /// `x+=y`
  AddAssign,
  /// `x-=y`
  SubAssign,
  /// `x*=y`
  MulAssign,
  /// `x/=y`
  DivAssign,
  /// `x%=y`
  RemAssign,
  /// `x&=y`
  BitAndAssign,
  /// `x|=y`
  BitOrAssign,
  /// `x^=y`
  BitXorAssign,
  /// `x>>=y`
  ShiftLeftAssign,
  /// `x<<=y`
  ShiftRightAssign,
}
impl From<InfiOp> for BinOpKind {
  fn from(value: InfiOp) -> Self {
    match value {
      InfiOp::Path => Self::Path,
      InfiOp::Access => Self::Access,
      InfiOp::Mul => Self::Mul,
      InfiOp::Div => Self::Div,
      InfiOp::Rem => Self::Rem,
      InfiOp::Add => Self::Add,
      InfiOp::Sub => Self::Sub,
      InfiOp::ShiftLeft => Self::ShiftLeft,
      InfiOp::ShiftRight => Self::ShiftRight,
      InfiOp::BitAnd => Self::BitAnd,
      InfiOp::BitXor => Self::BitXor,
      InfiOp::BitOr => Self::BitOr,
      InfiOp::CmpEq => Self::CmpEq,
      InfiOp::CmpNe => Self::CmpNe,
      InfiOp::CmpLt => Self::CmpLt,
      InfiOp::CmpGt => Self::CmpGt,
      InfiOp::CmpLe => Self::CmpLe,
      InfiOp::CmpGe => Self::CmpGe,
      InfiOp::ConditionalAnd => Self::ConditionalAnd,
      InfiOp::ConditionalOr => Self::ConditionalOr,
      InfiOp::RangeExclusive => Self::RangeExclusive,
      InfiOp::RangeInclusive => Self::RangeInclusive,
      InfiOp::Assign => Self::Assign,
      InfiOp::AddAssign => Self::AddAssign,
      InfiOp::SubAssign => Self::SubAssign,
      InfiOp::MulAssign => Self::MulAssign,
      InfiOp::DivAssign => Self::DivAssign,
      InfiOp::RemAssign => Self::RemAssign,
      InfiOp::BitAndAssign => Self::BitAndAssign,
      InfiOp::BitOrAssign => Self::BitOrAssign,
      InfiOp::BitXorAssign => Self::BitXorAssign,
      InfiOp::ShiftLeftAssign => Self::ShiftLeftAssign,
      InfiOp::ShiftRightAssign => Self::ShiftRightAssign,
    }
  }
}

/// A unary (one-value) operator.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnOpKind {
  /// `x?`
  Try,
  /// `x..`
  PostfixRangeExclusive,
  /// `x..=`
  PostfixRangeInclusive,
  /// `-x`
  Negative,
  /// `!x`
  BitNot,
  /// `*x`
  Dereference,
  /// `&x`
  Reference,
  /// `return x`
  Return,
  /// `..x`
  PrefixRangeExclusive,
  /// `..=x`
  PrefixRangeInclusive,
}
