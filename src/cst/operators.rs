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
pub enum PrefixOperator {
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
impl PrefixOperator {
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
pub enum PostfixOperator {
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
impl PostfixOperator {
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
pub enum InfixOperator {
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
impl InfixOperator {
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
      InfixOperator::ShiftLeftAssign | InfixOperator::ShiftRightAssign => 3,
      InfixOperator::ShiftLeft
      | InfixOperator::ShiftRight
      | InfixOperator::ConditionalAnd
      | InfixOperator::ConditionalOr
      | InfixOperator::CmpGe
      | InfixOperator::CmpLe => 2,
      _ => 1,
    }
  }
}
