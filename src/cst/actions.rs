#![allow(dead_code)]
//! Module for free functions that manipulate a [CstParser] to build a useful
//! [Cst]

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
pub fn do_module(p: &mut CstParser) {
  let m_module = p.open();
  loop {
    while let Whitespace = p.peek() {
      p.advance();
    }
    let m_item = p.open_eat_trivia();
    match p.peek() {
      ErrEndOfFile => {
        p.abandon_subtree(m_item);
        p.close(m_module, CstKind::Module);
        return;
      }
      k if ITEM_KEYWORDS.contains(&k) => {
        do_item(p);
        while let Whitespace = p.peek() {
          p.advance();
        }
        p.close(m_item, CstKind::Item);
      }
      _ => {
        while p.has_more() {
          match p.peek() {
            Comment => break,
            x if ITEM_KEYWORDS.contains(&x) => break,
            _ => {
              p.advance();
            }
          }
        }
        p.close(m_item, CstKind::ErrCstKind);
      }
    }
  }
}

/// Eats one item.
///
/// * Panics if `p.peek()` isn't an item keyword.
fn do_item(p: &mut CstParser<'_>) {
  match p.peek() {
    KwUse => do_use(p),
    KwStruct => do_struct(p),
    KwBitbag => do_bitbag(p),
    KwEnum => do_enum(p),
    KwStatic => do_static(p),
    KwConst => do_const(p),
    KwFn => do_fn(p),
    KwImpl => do_impl(p),
    other => panic!("{other:?}"),
  }
}

fn do_impl(p: &mut CstParser<'_>) {
  p.expect(KwImpl);
  p.eat_trivia();
  p.expect(Ident);
  p.eat_trivia();
  let m_brace = p.open();
  p.expect(OpBrace);
  loop {
    while let Whitespace = p.peek() {
      p.advance();
    }
    let m_item = p.open_eat_trivia();
    match p.peek() {
      ErrEndOfFile => {
        p.abandon_subtree(m_item);
        // todo: log error about a missing close brace.
        p.close(m_brace, CstKind::BraceGroup);
        return;
      }
      ClBrace => {
        p.abandon_subtree(m_item);
        p.advance();
        p.close(m_brace, CstKind::BraceGroup);
        return;
      }
      k if ITEM_KEYWORDS.contains(&k) => {
        do_item(p);
        while let Whitespace = p.peek() {
          p.advance();
        }
        p.close(m_item, CstKind::Item);
      }
      _ => {
        while p.has_more() {
          match p.peek() {
            Comment => break,
            x if ITEM_KEYWORDS.contains(&x) => break,
            _ => {
              p.advance();
            }
          }
        }
        p.close(m_item, CstKind::ErrCstKind);
      }
    }
  }
}

fn do_fn(p: &mut CstParser<'_>) {
  p.expect(KwFn);
  p.eat_trivia();
  p.expect(Ident);
  p.eat_trivia();
  let m = p.open();
  p.expect(OpParen);
  while p.has_more() && p.peek() != ClParen {
    p.advance();
  }
  p.expect(ClParen);
  p.close(m, CstKind::ParensGroup);
  p.eat_trivia();
  if p.peek() == MinusGreater {
    p.advance();
    p.eat_trivia();
    p.expect(Ident);
    p.eat_trivia();
  }
  let m = p.open();
  p.expect(OpBrace);
  while p.has_more() && p.peek() != ClBrace {
    p.advance();
  }
  p.expect(ClBrace);
  p.close(m, CstKind::BraceGroup);
}

fn do_const(p: &mut CstParser<'_>) {
  p.expect(KwConst);
  let mut bracket_depth = 0_isize;
  let mut brackets = Vec::new();
  while p.has_more() {
    match p.peek() {
      Semicolon if bracket_depth == 0 => break,
      OpBracket => {
        bracket_depth += 1;
        brackets.push(p.open());
      }
      ClBracket => {
        bracket_depth -= 1;
        if let Some(mark) = brackets.pop() {
          p.advance();
          p.close(mark, CstKind::BracketGroup);
          continue;
        }
      }
      _ => (),
    }
    p.advance();
  }
  p.advance();
}

fn do_static(p: &mut CstParser<'_>) {
  p.expect(KwStatic);
  let mut bracket_depth = 0_isize;
  let mut brackets = Vec::new();
  while p.has_more() {
    match p.peek() {
      Semicolon if bracket_depth == 0 => break,
      OpBracket => {
        bracket_depth += 1;
        brackets.push(p.open());
      }
      ClBracket => {
        bracket_depth -= 1;
        if let Some(mark) = brackets.pop() {
          p.advance();
          p.close(mark, CstKind::BracketGroup);
          continue;
        }
      }
      _ => (),
    }
    p.advance();
  }
  p.advance();
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
  while p.has_more() && p.peek() != ClBrace {
    p.advance();
  }
  p.expect(ClBrace);
  p.close(m_braces, CstKind::BraceGroup);
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
        do_expr_type(p);
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

fn do_expr_type(p: &mut CstParser<'_>) {
  let m_ty = p.open();
  match p.peek() {
    Comment | Whitespace => panic!(),
    Ident => {
      p.expect(Ident);
    }
    _ => {
      todo!();
    }
  }
  p.close(m_ty, CstKind::ExprType);
}

/// Parse a value expression, or `None` for no input consumed.
fn do_value_expr(p: &mut CstParser) {
  try_value_expr_rec(p, 0);
  return;

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

  // todo: i think if and loop need to be parsable as expression atoms
  /// Parse a value atom, or `None` for no input consumed.
  fn try_val_atom(p: &mut CstParser) -> Option<CloseMark> {
    debug_assert_ne!(p.peek(), Whitespace);
    debug_assert_ne!(p.peek(), Comment);
    Some(match p.peek() {
      KwTrue | KwFalse | Ident | LitNum | LitStr => {
        let m = p.open();
        p.advance();
        p.close(m, CstKind::ExprVal)
      }
      OpParen => {
        let m = p.open();
        p.expect(OpParen);
        p.eat_trivia();
        try_value_expr_rec(p, 0);
        p.eat_trivia();
        p.expect(ClParen);
        p.close(m, CstKind::ExprVal)
      }
      KwLoop => {
        let m_expr = p.open();
        do_loop(p, m_expr)
      }
      KwIf => {
        let m_expr = p.open();
        do_if(p, m_expr)
      }
      KwFor => {
        let m_expr = p.open();
        do_for(p, m_expr)
      }
      _ => return None,
    })
  }

  /// recrusive form, where you also pass the pratt bind power from the parent
  /// context.
  fn try_value_expr_rec(p: &mut CstParser, min_bp: u8) -> Option<CloseMark> {
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
      if try_value_expr_rec(p, op.binding()).is_none() && op.needs_operand() {
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
              if let Some(_xpr_mark) = try_value_expr_rec(p, bind_power) {
                p.eat_trivia();
                if p.peek() == Comma {
                  p.expect(TokenKind::Comma);
                  p.eat_trivia();
                }
              } else {
                break;
              }
            }
            p.close(arg_list_mark, CstKind::ParensGroup);
            p.expect(TokenKind::ClParen);
          }
          PostfixOperator::ArrayIndex => {
            let arg_list_mark = p.open();
            p.eat_trivia();
            if try_value_expr_rec(p, 0).is_none() {
              // todo: log error
            }
            p.eat_trivia();
            p.close(arg_list_mark, CstKind::ExprVal);
            p.expect(TokenKind::ClBracket);
          }
          PostfixOperator::As => {
            p.eat_trivia();
            do_expr_type(p);
            p.eat_trivia();
          }
          PostfixOperator::PostfixRangeExclusive
          | PostfixOperator::PostfixRangeInclusive => {
            p.eat_trivia();
            try_value_expr_rec(p, rhs_bp);
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
        if try_value_expr_rec(p, rhs_bp).is_none() {
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
}

fn do_loop(p: &mut CstParser, mark: OpenMark) -> CloseMark {
  debug_assert_eq!(p.peek(), KwLoop);
  p.expect(KwLoop);
  let m_body = p.open_eat_trivia();
  if p.peek() == OpBrace {
    do_body(p, m_body);
  } else {
    p.close(m_body, CstKind::ErrCstKind);
  }
  p.close(mark, CstKind::ExprVal)
}

fn do_if(p: &mut CstParser, mark: OpenMark) -> CloseMark {
  debug_assert_eq!(p.peek(), KwIf);
  p.expect(KwIf);
  p.eat_trivia();
  do_value_expr(p);
  let m_body = p.open_eat_trivia();
  if p.peek() == OpBrace {
    do_body(p, m_body);
  } else {
    p.close(m_body, CstKind::ErrCstKind);
  }
  // TODO: handle "else"
  p.close(mark, CstKind::ExprVal)
}

fn do_for(p: &mut CstParser, m_expr: OpenMark) -> CloseMark {
  debug_assert_eq!(p.peek(), KwFor);
  p.expect(KwFor);
  p.eat_trivia();
  do_value_expr(p);
  p.expect(KwIn);
  do_value_expr(p);
  let m_body = p.open_eat_trivia();
  if p.peek() == OpBrace {
    do_body(p, m_body);
  } else {
    p.close(m_body, CstKind::ErrCstKind);
  }
  p.close(m_expr, CstKind::ExprVal)
}

fn do_body(p: &mut CstParser, m_body: OpenMark) {
  debug_assert_ne!(p.peek(), Whitespace);
  debug_assert_ne!(p.peek(), Comment);
  debug_assert_ne!(p.peek(), ErrEndOfFile);
  p.expect(OpBrace);
  loop {
    let m_stmt = p.open_eat_trivia();
    if p.peek() == ClBrace {
      p.abandon_subtree(m_stmt);
      p.expect(ClBrace);
      p.close(m_body, CstKind::BraceGroup);
      return;
    }
    if p.peek() == ErrEndOfFile {
      p.close(m_stmt, CstKind::Statement);
      p.close(m_body, CstKind::ErrCstKind);
      return;
    }
    do_stmt(p, m_stmt);
  }
}

fn do_stmt(p: &mut CstParser, m_stmt: OpenMark) {
  debug_assert_ne!(p.peek(), Whitespace);
  debug_assert_ne!(p.peek(), Comment);
  debug_assert_ne!(p.peek(), ErrEndOfFile);
  match p.peek() {
    Semicolon => {
      p.advance();
      p.close(m_stmt, CstKind::Statement);
    }
    KwLet => {
      p.expect(KwLet);
      p.eat_trivia();
      p.expect(Ident);
      p.eat_trivia();
      p.expect(Equal);
      p.eat_trivia();
      do_value_expr(p);
      p.expect(Semicolon);
      p.close(m_stmt, CstKind::Statement);
    }
    // Keywords that start an expression which ends with a brace, which have an
    // implied semicolon after the brace, need to be caught here and given that
    // separate handling, instead of passing to the generic expression statement
    // handling which doesn't know about the implied semicolon.
    KwLoop => {
      let m_expr = p.open();
      do_loop(p, m_expr);
      p.close(m_stmt, CstKind::Statement);
    }
    KwIf => {
      let m_expr = p.open();
      do_if(p, m_expr);
      p.close(m_stmt, CstKind::Statement);
    }
    KwFor => {
      let m_expr = p.open();
      do_for(p, m_expr);
      p.close(m_stmt, CstKind::Statement);
    }
    _ => {
      do_value_expr(p);
      p.expect(Semicolon);
      p.close(m_stmt, CstKind::Statement);
    }
  }
}
