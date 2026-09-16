#![allow(dead_code)]
//! Module for free functions that manipulate a [CstParser] to build a useful
//! [Cst]
//!
//! ## Conventions
//! * All actions that need to be within a grouping should have the grouping
//!   opened and closed by the *caller* of the action.

use std::ops::ControlFlow;

use crate::cst::CstKind::{self};
use crate::cst::parser::{CloseMark, CstParser, OpenMark};
use crate::operators::{
  BindDirection, InfixOperator, PostfixOperator, PrefixOperator,
};
use crate::tokenizer::TokenKind::*;
use crate::tokenizer::{Token, TokenKind, tokenize};

static ITEM_KEYWORDS: &[TokenKind] =
  &[KwUse, KwStruct, KwBitbag, KwEnum, KwStatic, KwConst, KwFn, KwImpl];

/// Parse an entire module's content.
///
/// * makes its own events.
pub fn gather_module(p: &mut CstParser<'_>) {
  let m = p.open();
  loop {
    // comments before an item are "part of" that item.
    let m_item = p.open_eat_trivia();
    match p.peek() {
      ErrEndOfFile => {
        p.abandon_subtree(m_item);
        break;
      }
      KwUse => do_use(p),
      KwStruct => do_struct(p),
      KwBitbag => do_bitbag(p),
      KwEnum => do_enum(p),
      KwStatic => do_static(p),
      KwConst => do_const(p),
      KwFn => do_fn(p),
      KwImpl => do_impl(p),
      _ => {
        // skip over everything until we see another potential item.
        loop {
          match p.peek() {
            ErrEndOfFile | KwUse | KwStruct | KwBitbag | KwEnum | KwStatic
            | KwConst | KwFn | KwImpl | Comment => break,
            _ => {
              p.advance();
            }
          }
        }
        // todo: log error
      }
    }
    p.close(m_item, CstKind::Item);
  }
  p.close(m, CstKind::Module);
}

fn do_impl(p: &mut CstParser<'_>) {
  p.expect(KwImpl);
  p.eat_trivia();
  p.expect(Ident);
  p.eat_trivia();
  let m_brace = p.open();
  p.expect(OpBrace);
  loop {
    let m_item = p.open_eat_trivia();
    match p.peek() {
      ErrEndOfFile => {
        p.abandon_subtree(m_item);
        // TODO: log error
        break;
      }
      ClBrace => {
        p.abandon_subtree(m_item);
        break;
      }
      KwUse => do_use(p),
      KwStruct => do_struct(p),
      KwBitbag => do_bitbag(p),
      KwEnum => do_enum(p),
      KwStatic => do_static(p),
      KwConst => do_const(p),
      KwFn => do_fn(p),
      KwImpl => do_impl(p),
      _ => {
        // skip over everything until we see another potential item or the end
        // of this braces group.
        loop {
          match p.peek() {
            ClBrace | ErrEndOfFile | KwUse | KwStruct | KwBitbag | KwEnum
            | KwStatic | KwConst | KwFn | KwImpl | Comment => break,
            _ => {
              p.advance();
            }
          }
        }
        // todo: log error
      }
    }
    p.close(m_item, CstKind::Item);
  }
  p.close(m_brace, CstKind::BraceGroup);
}

fn do_fn(p: &mut CstParser<'_>) {
  p.expect(KwFn);
  p.eat_trivia();
  p.expect(Ident);
  p.eat_trivia();
  let m = p.open();
  p.expect(OpParen);
  p.eat_trivia();
  loop {
    if p.peek() == ClParen {
      break;
    }
    // todo: allow parsing `self`, `&self`, and `&mut self` as function
    // arguments.
    gather_pattern(p);
    p.eat_trivia();
    p.expect(Colon);
    p.eat_trivia();
    gather_expr_type(p);
    p.eat_trivia();
    if p.peek() == Comma {
      p.expect(Comma);
    }
  }
  p.expect(ClParen);
  p.close(m, CstKind::ParensGroup);
  p.eat_trivia();
  if p.peek() == MinusGreater {
    p.advance();
    p.eat_trivia();
    gather_expr_type(p);
    p.eat_trivia();
  }
  gather_body(p);
}

fn do_const(p: &mut CstParser<'_>) {
  p.expect(KwConst);
  p.eat_trivia();
  p.expect(Ident);
  p.eat_trivia();
  p.expect(Colon);
  p.eat_trivia();
  gather_expr_type(p);
  p.eat_trivia();
  p.expect(Equal);
  p.eat_trivia();
  gather_expr_value(p);
  p.expect(Semicolon);
}

fn do_static(p: &mut CstParser<'_>) {
  p.expect(KwStatic);
  p.eat_trivia();
  match p.peek() {
    KwMmio => {
      p.expect(KwMmio);
      p.eat_trivia();
      p.expect(OpParen);
      p.eat_trivia();
      gather_expr_value(p);
      p.eat_trivia();
      p.expect(ClParen);
      p.eat_trivia();
      p.expect(Ident);
      p.eat_trivia();
      p.expect(Colon);
      p.eat_trivia();
      gather_expr_type(p);
      p.eat_trivia();
      p.expect(Semicolon);
    }
    KwRam => {
      p.expect(KwRam);
      p.eat_trivia();
      p.expect(Ident);
      p.eat_trivia();
      p.expect(Colon);
      p.eat_trivia();
      gather_expr_type(p);
      p.eat_trivia();
      p.expect(Equal);
      p.eat_trivia();
      gather_expr_value(p);
      p.expect(Semicolon);
    }
    KwRom => {
      p.expect(KwRom);
      p.eat_trivia();
      p.expect(Ident);
      p.eat_trivia();
      p.expect(Colon);
      p.eat_trivia();
      gather_expr_type(p);
      p.eat_trivia();
      p.expect(Equal);
      p.eat_trivia();
      gather_expr_value(p);
      p.expect(Semicolon);
    }
    _ => {
      // skip over everything until we see another potential item.
      loop {
        match p.peek() {
          ErrEndOfFile | KwUse | KwStruct | KwBitbag | KwEnum | KwStatic
          | KwConst | KwFn | KwImpl | Comment => break,
          _ => {
            p.advance();
          }
        }
      }
      // todo: log error
    }
  }
}

fn do_enum(p: &mut CstParser<'_>) {
  p.expect(KwEnum);
  p.eat_trivia();
  p.expect(Ident);
  p.eat_trivia();
  let m_braces = p.open();
  p.expect(OpBrace);
  while p.has_more() && p.peek() != ClBrace {
    p.advance();
  }
  p.expect(ClBrace);
  p.close(m_braces, CstKind::BraceGroup);
}

fn do_bitbag(p: &mut CstParser<'_>) {
  p.expect(KwBitbag);
  p.eat_trivia();
  p.expect(Ident);
  p.eat_trivia();
  let m_braces = p.open();
  p.expect(OpBrace);
  loop {
    p.eat_trivia();
    match p.peek() {
      ErrEndOfFile => {
        // todo: log error about no close brace
        p.close(m_braces, CstKind::BraceGroup);
        return;
      }
      ClBrace => {
        p.expect(ClBrace);
        p.close(m_braces, CstKind::BraceGroup);
        return;
      }
      _ => {
        p.expect(Ident);
        p.eat_trivia();
        p.expect(Colon);
        p.eat_trivia();
        gather_expr_value(p);
        p.eat_trivia();
        match p.peek() {
          ClBrace => {
            p.expect(ClBrace);
            p.close(m_braces, CstKind::BraceGroup);
            return;
          }
          _ => {
            p.expect(Comma);
          }
        }
      }
    }
  }
}

fn do_struct(p: &mut CstParser<'_>) {
  p.expect(KwStruct);
  p.eat_trivia();
  p.expect(Ident);
  p.eat_trivia();
  let m_braces = p.open();
  p.expect(OpBrace);
  loop {
    p.eat_trivia();
    match p.peek() {
      ErrEndOfFile => {
        // todo: log error about no close brace
        p.close(m_braces, CstKind::BraceGroup);
        return;
      }
      ClBrace => {
        p.expect(ClBrace);
        p.close(m_braces, CstKind::BraceGroup);
        return;
      }
      _ => {
        p.expect(Ident);
        p.eat_trivia();
        p.expect(Colon);
        p.eat_trivia();
        gather_expr_type(p);
        p.eat_trivia();
        match p.peek() {
          ClBrace => {
            p.expect(ClBrace);
            p.close(m_braces, CstKind::BraceGroup);
            return;
          }
          _ => {
            p.expect(Comma);
          }
        }
      }
    }
  }
}

fn do_use(p: &mut CstParser<'_>) {
  p.expect(KwUse);
  while p.has_more() && p.peek() != Semicolon {
    p.advance();
  }
  p.advance();
}

fn gather_expr_type(p: &mut CstParser<'_>) {
  let m_ty = p.open();
  match p.peek() {
    Comment | Whitespace => panic!(),
    Ident => {
      p.expect(Ident);
    }
    OpBracket => {
      p.expect(OpBracket);
      p.eat_trivia();
      gather_expr_type(p);
      p.eat_trivia();
      p.expect(Semicolon);
      p.eat_trivia();
      gather_expr_value(p);
      p.eat_trivia();
      p.expect(ClBracket);
    }
    other => {
      todo!("{other:?}");
    }
  }
  p.close(m_ty, CstKind::ExprType);
}

/// Checks for a [PrefixOperator]
fn peek_prefix_operator(p: &mut CstParser<'_>) -> Option<PrefixOperator> {
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
fn peek_infix_operator(p: &mut CstParser<'_>) -> Option<InfixOperator> {
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
fn peek_postfix_operator(p: &mut CstParser<'_>) -> Option<PostfixOperator> {
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
fn try_val_atom(p: &mut CstParser<'_>) -> Option<CloseMark> {
  debug_assert_ne!(p.peek(), Whitespace);
  debug_assert_ne!(p.peek(), Comment);
  Some(match p.peek() {
    KwTrue | KwFalse | LitNum | LitStr => {
      let m = p.open();
      p.advance();
      p.close(m, CstKind::ExprVal)
    }
    Ident => {
      // TODO: allow for struct literal expressions here.
      let m = p.open();
      p.advance();
      p.close(m, CstKind::ExprVal)
    }
    OpParen => {
      let m = p.open();
      p.expect(OpParen);
      p.eat_trivia();
      try_expr_value_rec(p, 0);
      p.eat_trivia();
      p.expect(ClParen);
      p.close(m, CstKind::ExprVal)
    }
    OpBrace => gather_body(p),
    KwLoop => gather_loop(p),
    KwIf => gather_if(p),
    KwFor => gather_for(p),
    // todo: array expressions
    _ => return None,
  })
}

/// recrusive form, where you also pass the pratt bind power from the parent
/// context.
fn try_expr_value_rec(p: &mut CstParser<'_>, min_bp: u8) -> Option<CloseMark> {
  debug_assert_ne!(p.peek(), Whitespace);
  debug_assert_ne!(p.peek(), Comment);
  // prefix or atom
  let mut lhs: CloseMark = if let Some(op) = peek_prefix_operator(p) {
    let lhs_mark = p.open();
    let op_mark = p.open();
    for _ in 0..op.token_length() {
      p.advance();
    }
    p.eat_trivia();
    if op == PrefixOperator::Break && p.peek() == Quote {
      p.expect(TokenKind::Quote);
      p.expect(TokenKind::Ident);
      p.eat_trivia();
    }
    p.close(op_mark, CstKind::OperatorPrefix(op));
    if try_expr_value_rec(p, op.binding()).is_none() && op.needs_operand() {
      // todo: log error
    }
    p.close(lhs_mark, CstKind::ExprVal)
  } else {
    try_val_atom(p)?
  };
  p.eat_trivia();
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
      let new_lhs = p.open_before(lhs);
      let op_mark = p.open();
      for _ in 0..op.token_length() {
        p.advance();
      }
      p.close(op_mark, CstKind::OperatorPostfix(op));
      match op {
        PostfixOperator::Try => (),
        PostfixOperator::FnCall => {
          let arg_list_mark = p.open();
          loop {
            p.eat_trivia();
            if let Some(_xpr_mark) = try_expr_value_rec(p, bind_power) {
              p.eat_trivia();
              if p.peek() == Comma {
                p.expect(TokenKind::Comma);
                p.eat_trivia();
              }
            } else {
              break;
            }
          }
          p.expect(TokenKind::ClParen);
          p.close(arg_list_mark, CstKind::ParensGroup);
        }
        PostfixOperator::ArrayIndex => {
          let arg_list_mark = p.open();
          p.eat_trivia();
          if try_expr_value_rec(p, 0).is_none() {
            // todo: log error
          }
          p.eat_trivia();
          p.close(arg_list_mark, CstKind::ExprVal);
          p.expect(TokenKind::ClBracket);
        }
        PostfixOperator::As => {
          p.eat_trivia();
          gather_expr_type(p);
          p.eat_trivia();
        }
        PostfixOperator::PostfixRangeExclusive
        | PostfixOperator::PostfixRangeInclusive => {
          p.eat_trivia();
          try_expr_value_rec(p, rhs_bp);
          p.eat_trivia();
        }
      }
      lhs = p.close(new_lhs, CstKind::ExprVal);
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
        // todo: log error
      }
      let new_lhs = p.open_before(lhs);
      let op_mark = p.open();
      for _ in 0..op.token_length() {
        p.advance();
      }
      p.close(op_mark, CstKind::OperatorInfix(op));
      p.eat_trivia();
      // rhs
      if try_expr_value_rec(p, rhs_bp).is_none() {
        // todo: log error
      }
      lhs = p.close(new_lhs, CstKind::ExprVal);
      continue;
    }
    // no operator visible, so we stop gathering.
    break;
  }
  Some(lhs)
}

/// Parse a value expression, or `None` for no input consumed.
fn gather_expr_value(p: &mut CstParser<'_>) {
  try_expr_value_rec(p, 0);
  return;
}

fn gather_loop(p: &mut CstParser<'_>) -> CloseMark {
  debug_assert_eq!(p.peek(), KwLoop);
  //
  let m = p.open();
  p.expect(KwLoop);
  p.eat_trivia();
  gather_body(p);
  p.close(m, CstKind::ExprVal)
}

fn gather_if(p: &mut CstParser<'_>) -> CloseMark {
  debug_assert_eq!(p.peek(), KwIf);
  //
  let m = p.open();
  p.expect(KwIf);
  p.eat_trivia();
  gather_expr_value(p);
  p.eat_trivia();
  gather_body(p);
  p.eat_trivia();
  if p.peek() == KwElse {
    p.expect(KwElse);
    p.eat_trivia();
    match p.peek() {
      KwIf => {
        gather_if(p);
      }
      OpBrace => {
        gather_body(p);
      }
      _ => {
        // todo: log error
      }
    }
  }
  p.close(m, CstKind::ExprVal)
}

fn gather_for(p: &mut CstParser<'_>) -> CloseMark {
  debug_assert_eq!(p.peek(), KwFor);
  //
  let m = p.open();
  p.expect(KwFor);
  p.eat_trivia();
  gather_expr_value(p);
  p.expect(KwIn);
  p.eat_trivia();
  gather_expr_value(p);
  gather_body(p);
  p.close(m, CstKind::ExprVal)
}

fn gather_body(p: &mut CstParser<'_>) -> CloseMark {
  debug_assert_ne!(p.peek(), Whitespace);
  debug_assert_ne!(p.peek(), Comment);
  debug_assert_ne!(p.peek(), ErrEndOfFile);
  let m = p.open();
  p.expect(OpBrace);
  loop {
    let m_stmt = p.open_eat_trivia();
    match p.peek() {
      ErrEndOfFile => {
        p.abandon_subtree(m_stmt);
        // todo: log error
        break;
      }
      ClBrace => {
        p.abandon_subtree(m_stmt);
        p.expect(ClBrace);
        break;
      }
      Semicolon => {
        p.advance();
      }
      KwLet => {
        p.expect(KwLet);
        p.eat_trivia();
        gather_pattern(p);
        p.eat_trivia();
        p.expect(Equal);
        p.eat_trivia();
        gather_expr_value(p);
        p.expect(Semicolon);
      }
      // Keywords that start an expression which ends with a brace, which have an
      // implied semicolon after the brace, need to be caught here and given that
      // separate handling, instead of passing to the generic expression statement
      // handling which doesn't know about the implied semicolon.
      KwLoop => {
        gather_loop(p);
      }
      KwIf => {
        gather_if(p);
      }
      KwFor => {
        gather_for(p);
      }
      // catch all for any other expression.
      _ => {
        gather_expr_value(p);
        p.eat_trivia();
        if p.peek() != ClBrace {
          p.expect(Semicolon);
        }
      }
    }
    p.close(m_stmt, CstKind::Statement);
  }
  p.close(m, CstKind::ExprVal)
}

fn gather_pattern(p: &mut CstParser<'_>) {
  let m = p.open();
  // todo: some day we could allow more forms of pattern
  p.expect(Ident);
  p.close(m, CstKind::Pattern);
}
