#![allow(unused)]

//! Module for all the parsing junk!

use crate::cst::CstKind;
use crate::cst::parser_core::{CloseMark, CstParser, CstParserErrorKind};
use crate::r;
use crate::tokenizer::TokenKind::*;
use crate::tokenizer::{Token, TokenKind, tokenize};

mod tests;

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
  pub const fn needs_operand(self) -> bool {
    !matches!(
      self,
      Self::Return
        | Self::Break
        | Self::PrefixRangeExclusive
        | Self::PrefixRangeInclusive
    )
  }
  pub const fn token_length(self) -> usize {
    1
  }
}

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

  fn token_length(&self) -> usize {
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

  fn token_length(&self) -> usize {
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

/// Checks for a **prefix** operator.
fn peek_prefix_operator(p: &mut CstParser) -> Option<PrefixOperator> {
  debug_assert_ne!(p.peek(), Whitespace);
  debug_assert_ne!(p.peek(), Comment);
  let op = match p.peek() {
    Minus => PrefixOperator::Negative,
    Bang => PrefixOperator::BitNot,
    Star => PrefixOperator::Dereference,
    Ampersand => PrefixOperator::Reference,
    KwReturn => PrefixOperator::Return,
    KwBreak => PrefixOperator::Break,
    DotDot => PrefixOperator::PrefixRangeExclusive,
    DotDotEqual => PrefixOperator::PrefixRangeInclusive,
    _ => return None,
  };
  Some(op)
}

/// Checks for an **infix** operator.
fn peek_infix_operator(p: &mut CstParser) -> Option<InfixOperator> {
  debug_assert_ne!(p.peek(), Whitespace);
  debug_assert_ne!(p.peek(), Comment);
  //
  let mut tokens = p.tokens_tail().map(|tk| tk.kind);
  let op = match tokens.next().unwrap_or(TokenKind::ErrEndOfFile) {
    ColonColon => InfixOperator::Path,
    Dot => InfixOperator::Access,
    Star => InfixOperator::Mul,
    Slash => InfixOperator::Div,
    Percent => InfixOperator::Rem,
    Plus => InfixOperator::Add,
    Minus => InfixOperator::Sub,
    AmpersandEqual => InfixOperator::BitAndAssign,
    PipeEqual => InfixOperator::BitOrAssign,
    Caret => InfixOperator::BitXor,
    CaretEqual => InfixOperator::BitXorAssign,
    Equal => InfixOperator::Assign,
    EqualEqual => InfixOperator::CmpEq,
    BangEqual => InfixOperator::CmpNe,
    DotDot => InfixOperator::RangeExclusive,
    DotDotEqual => InfixOperator::RangeInclusive,
    PlusEqual => InfixOperator::AddAssign,
    MinusEqual => InfixOperator::SubAssign,
    StarEqual => InfixOperator::MulAssign,
    SlashEqual => InfixOperator::DivAssign,
    PercentEqual => InfixOperator::RemAssign,
    AmpersandEqual => InfixOperator::BitAndAssign,
    PipeEqual => InfixOperator::BitOrAssign,
    CaretEqual => InfixOperator::BitXorAssign,
    LessThan => {
      return Some(match tokens.next().unwrap_or(TokenKind::ErrEndOfFile) {
        LessThan => {
          return Some(
            match tokens.next().unwrap_or(TokenKind::ErrEndOfFile) {
              Equal => InfixOperator::ShiftLeftAssign,
              _ => InfixOperator::ShiftLeft,
            },
          );
        }
        Equal => InfixOperator::CmpLe,
        _ => InfixOperator::CmpLt,
      });
    }
    GreaterThan => {
      return Some(match tokens.next().unwrap_or(TokenKind::ErrEndOfFile) {
        GreaterThan => {
          return Some(
            match tokens.next().unwrap_or(TokenKind::ErrEndOfFile) {
              Equal => (InfixOperator::ShiftRightAssign),
              _ => (InfixOperator::ShiftRight),
            },
          );
        }
        Equal => InfixOperator::CmpGe,
        _ => InfixOperator::CmpGt,
      });
    }
    Ampersand => {
      return Some(match tokens.next().unwrap_or(TokenKind::ErrEndOfFile) {
        Ampersand => InfixOperator::ConditionalAnd,
        _ => InfixOperator::BitAnd,
      });
    }
    Pipe => {
      return Some(match tokens.next().unwrap_or(TokenKind::ErrEndOfFile) {
        Pipe => InfixOperator::ConditionalOr,
        _ => InfixOperator::BitOr,
      });
    }
    _ => return None,
  };
  Some(op)
}

/// Checks for a **postfix** operator.
fn peek_postfix_operator(p: &mut CstParser) -> Option<PostfixOperator> {
  debug_assert_ne!(p.peek(), Whitespace);
  debug_assert_ne!(p.peek(), Comment);
  let op = match p.peek() {
    OpParen => PostfixOperator::FnCall,
    OpBracket => PostfixOperator::ArrayIndex,
    Question => PostfixOperator::Try,
    KwAs => PostfixOperator::As,
    DotDot => PostfixOperator::PostfixRangeExclusive,
    DotDotEqual => PostfixOperator::PostfixRangeInclusive,
    _ => return None,
  };
  Some(op)
}

/// Parse a value atom, or `None` for no input consumed.
fn try_val_atom(p: &mut CstParser) -> Option<CloseMark> {
  debug_assert_ne!(p.peek(), Whitespace);
  debug_assert_ne!(p.peek(), Comment);
  Some(match p.peek() {
    KwTrue | KwFalse | Ident | LitNum | LitStr => {
      let m = p.open();
      p.advance();
      p.close(m, CstKind::AtomicValue)
    }
    OpParen => {
      let m = p.open();
      p.expect(OpParen);
      p.advance_over_whitespace_and_comments();
      try_val_expr(p, 0);
      p.advance_over_whitespace_and_comments();
      p.expect(ClParen);
      p.close(m, CstKind::ParenGroup)
    }
    _ => return None,
  })
}

/// try parsing a type expression
fn try_type_expr(p: &mut CstParser) -> Option<CloseMark> {
  if p.at(TokenKind::Ident) {
    let m = p.open();
    p.expect(TokenKind::Ident);
    Some(p.close(m, CstKind::TypeExpr))
  } else {
    None
  }
}

/// Parse a value expression, or `None` for no input consumed.
fn try_val_expr(p: &mut CstParser, min_bp: u8) -> Option<CloseMark> {
  debug_assert_ne!(p.peek(), Whitespace);
  debug_assert_ne!(p.peek(), Comment);
  // prefix or atom
  let mut lhs: CloseMark = if let Some(op) = peek_prefix_operator(p) {
    let bind_power = op.binding();
    let expr_mark = p.open();
    let op_mark = p.open();
    for _ in 0..op.token_length() {
      p.advance();
    }
    p.advance_over_whitespace_and_comments();
    if op == PrefixOperator::Break && p.at(TokenKind::Quote) {
      p.expect(TokenKind::Quote);
      p.expect(TokenKind::Ident);
      p.advance_over_whitespace_and_comments();
    }
    p.close(op_mark, CstKind::PrefixOperator);
    if try_val_expr(p, bind_power).is_none() && op.needs_operand() {
      let m2 = p.open();
      p.close(m2, CstKind::ErrExpectedValueExpression);
    }
    p.close(expr_mark, CstKind::ValExpr)
  } else {
    try_val_atom(p)?
  };
  p.advance_over_whitespace_and_comments();
  // infix/postfix looping
  let mut previous_bind_power: Option<u8> = None;
  loop {
    if let Some(op) = peek_postfix_operator(p) {
      let bind_power = op.binding();
      let (lhs_bp, rhs_bp) = (bind_power, bind_power + 1);
      if lhs_bp < min_bp {
        // caller's operator, don't consume it
        break;
      }
      previous_bind_power = Some(bind_power);
      let expr_mark = p.open_before(lhs);
      let op_mark = p.open();
      for _ in 0..op.token_length() {
        p.advance();
      }
      p.close(op_mark, CstKind::PostfixOperator);
      match op {
        PostfixOperator::Try => (),
        PostfixOperator::FnCall => {
          let arg_list_mark = p.open();
          loop {
            p.advance_over_whitespace_and_comments();
            if let Some(xpr_mark) = try_val_expr(p, bind_power) {
              p.advance_over_whitespace_and_comments();
              if p.at(TokenKind::Comma) {
                p.expect(TokenKind::Comma);
                p.advance_over_whitespace_and_comments();
              }
            } else {
              break;
            }
          }
          p.close(arg_list_mark, CstKind::ArgumentList);
          p.expect(TokenKind::ClParen);
        }
        PostfixOperator::ArrayIndex => {
          let arg_list_mark = p.open();
          p.advance_over_whitespace_and_comments();
          if try_val_expr(p, 0).is_none() {
            let err_mark = p.open();
            p.close(err_mark, CstKind::ErrExpectedValueExpression);
          }
          p.advance_over_whitespace_and_comments();
          p.close(arg_list_mark, CstKind::ValExpr);
          p.expect(TokenKind::ClBracket);
        }
        PostfixOperator::As => {
          p.advance_over_whitespace_and_comments();
          if try_type_expr(p).is_none() {
            let err_mark = p.open();
            p.close(err_mark, CstKind::ErrExpectedTypeExpression);
          }
          p.advance_over_whitespace_and_comments();
        }
        PostfixOperator::PostfixRangeExclusive
        | PostfixOperator::PostfixRangeInclusive => {
          p.advance_over_whitespace_and_comments();
          try_val_expr(p, rhs_bp);
          p.advance_over_whitespace_and_comments();
        }
      }
      lhs = p.close(expr_mark, CstKind::ValExpr);
      continue;
    }
    if let Some(op) = peek_infix_operator(p) {
      let bind_power = op.binding();
      let (lhs_bp, rhs_bp) = match op.direction() {
        BindDirection::Left => (bind_power, bind_power + 1),
        BindDirection::Right => (bind_power + 1, bind_power),
        BindDirection::Ambiguious => (bind_power, bind_power + 1),
      };
      if lhs_bp < min_bp {
        // caller's operator, don't consume it
        break;
      }
      if op.direction() == BindDirection::Ambiguious
        && previous_bind_power == Some(bind_power)
      {
        let err_mark = p.open();
        p.close(err_mark, CstKind::ErrNeedsParensToDisambiguate);
      }
      let expr_mark = p.open_before(lhs);
      let op_mark = p.open();
      for _ in 0..op.token_length() {
        p.advance();
      }
      p.close(op_mark, CstKind::InfixOperator);
      p.advance_over_whitespace_and_comments();
      // rhs
      if try_val_expr(p, rhs_bp).is_none() {
        let err_mark = p.open();
        p.close(err_mark, CstKind::ErrExpectedValueExpression);
      }
      lhs = p.close(expr_mark, CstKind::ValExpr);
      continue;
    }
    // no operator visible, so we stop gathering.
    break;
  }
  Some(lhs)
}
