#![allow(dead_code)]
//! Module for all the parsing junk!

use crate::cst::CstKind;
use crate::cst::operators::{
  BindDirection, InfixOperator, PostfixOperator, PrefixOperator,
};
use crate::cst::parser::{CloseMark, CstParser, CstParserErrorKind, OpenMark};
use crate::r;
use crate::tokenizer::TokenKind::*;
use crate::tokenizer::{Token, TokenKind, tokenize};

mod tests;

/// try parsing a type expression
pub fn try_type_expr(p: &mut CstParser) -> Option<CloseMark> {
  if p.at(TokenKind::Ident) {
    let m = p.open();
    p.expect(TokenKind::Ident);
    Some(p.close(m, CstKind::TypeExpr))
  } else {
    None
  }
}

/// Checks for a [PrefixOperator]
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

/// Checks for an [InfixOperator]
fn peek_infix_operator(p: &mut CstParser) -> Option<InfixOperator> {
  debug_assert_ne!(p.peek(), Whitespace);
  debug_assert_ne!(p.peek(), Comment);
  //
  let mut token_kinds = p.tokens_tail();
  let op = match token_kinds.next().unwrap_or(TokenKind::ErrEndOfFile) {
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
    LessThan => {
      return Some(
        match token_kinds.next().unwrap_or(TokenKind::ErrEndOfFile) {
          LessThan => {
            return Some(
              match token_kinds.next().unwrap_or(TokenKind::ErrEndOfFile) {
                Equal => InfixOperator::ShiftLeftAssign,
                _ => InfixOperator::ShiftLeft,
              },
            );
          }
          Equal => InfixOperator::CmpLe,
          _ => InfixOperator::CmpLt,
        },
      );
    }
    GreaterThan => {
      return Some(
        match token_kinds.next().unwrap_or(TokenKind::ErrEndOfFile) {
          GreaterThan => {
            return Some(
              match token_kinds.next().unwrap_or(TokenKind::ErrEndOfFile) {
                Equal => (InfixOperator::ShiftRightAssign),
                _ => (InfixOperator::ShiftRight),
              },
            );
          }
          Equal => InfixOperator::CmpGe,
          _ => InfixOperator::CmpGt,
        },
      );
    }
    Ampersand => {
      return Some(
        match token_kinds.next().unwrap_or(TokenKind::ErrEndOfFile) {
          Ampersand => InfixOperator::ConditionalAnd,
          _ => InfixOperator::BitAnd,
        },
      );
    }
    Pipe => {
      return Some(
        match token_kinds.next().unwrap_or(TokenKind::ErrEndOfFile) {
          Pipe => InfixOperator::ConditionalOr,
          _ => InfixOperator::BitOr,
        },
      );
    }
    _ => return None,
  };
  Some(op)
}

/// Checks for a [PostfixOperator]
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
      try_value_expr(p);
      p.advance_over_whitespace_and_comments();
      p.expect(ClParen);
      p.close(m, CstKind::ParenGroup)
    }
    _ => return None,
  })
}

/// Parse a value expression, or `None` for no input consumed.
pub fn try_value_expr(p: &mut CstParser) -> Option<CloseMark> {
  return try_value_expr_rec(p, 0);

  /// recrusive form, where you also pass the pratt bind power from the parent
  /// context.
  fn try_value_expr_rec(p: &mut CstParser, min_bp: u8) -> Option<CloseMark> {
    debug_assert_ne!(p.peek(), Whitespace);
    debug_assert_ne!(p.peek(), Comment);
    // prefix or atom
    let mut lhs: CloseMark = if let Some(op) = peek_prefix_operator(p) {
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
      if try_value_expr_rec(p, op.binding()).is_none() && op.needs_operand() {
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
              if let Some(_xpr_mark) = try_value_expr_rec(p, bind_power) {
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
            if try_value_expr_rec(p, 0).is_none() {
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
            try_value_expr_rec(p, rhs_bp);
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
        if try_value_expr_rec(p, rhs_bp).is_none() {
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
}
