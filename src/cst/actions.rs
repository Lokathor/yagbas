#![allow(dead_code)]
//! Module for all the parsing junk!

use crate::cst::CstKind::{self, ErrExpectedIfCondition};
use crate::cst::operators::{
  BindDirection, InfixOperator, PostfixOperator, PrefixOperator,
};
use crate::cst::parser::{CloseMark, CstParser, OpenMark};
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

/// Parse a value expression, or `None` for no input consumed.
pub fn try_value_expr(p: &mut CstParser) -> Option<CloseMark> {
  return try_value_expr_rec(p, 0);

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
        p.close(m, CstKind::AtomicValue)
      }
      OpParen => {
        let m = p.open();
        p.expect(OpParen);
        p.eat_trivia();
        try_value_expr(p);
        p.eat_trivia();
        p.expect(ClParen);
        p.close(m, CstKind::ParenGroup)
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
      let expr_mark = p.open();
      let op_mark = p.open();
      for _ in 0..op.token_length() {
        p.advance();
      }
      p.eat_trivia();
      if op == PrefixOperator::Break && p.at(TokenKind::Quote) {
        p.expect(TokenKind::Quote);
        p.expect(TokenKind::Ident);
        p.eat_trivia();
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
              p.eat_trivia();
              if let Some(_xpr_mark) = try_value_expr_rec(p, bind_power) {
                p.eat_trivia();
                if p.at(TokenKind::Comma) {
                  p.expect(TokenKind::Comma);
                  p.eat_trivia();
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
            p.eat_trivia();
            if try_value_expr_rec(p, 0).is_none() {
              let err_mark = p.open();
              p.close(err_mark, CstKind::ErrExpectedValueExpression);
            }
            p.eat_trivia();
            p.close(arg_list_mark, CstKind::ValExpr);
            p.expect(TokenKind::ClBracket);
          }
          PostfixOperator::As => {
            p.eat_trivia();
            if try_type_expr(p).is_none() {
              let err_mark = p.open();
              p.close(err_mark, CstKind::ErrExpectedTypeExpression);
            }
            p.eat_trivia();
          }
          PostfixOperator::PostfixRangeExclusive
          | PostfixOperator::PostfixRangeInclusive => {
            p.eat_trivia();
            try_value_expr_rec(p, rhs_bp);
            p.eat_trivia();
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
        p.eat_trivia();
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

pub fn do_stmt(p: &mut CstParser, m_stmt: OpenMark) -> CloseMark {
  debug_assert_ne!(p.peek(), Whitespace);
  debug_assert_ne!(p.peek(), Comment);
  debug_assert_ne!(p.peek(), ErrEndOfFile);
  match p.peek() {
    Semicolon => {
      p.advance();
      p.close(m_stmt, CstKind::StmtEmpty)
    }
    KwLet => {
      p.expect(KwLet);
      p.eat_trivia();
      p.expect(Ident);
      p.eat_trivia();
      p.expect(Equal);
      p.eat_trivia();
      try_value_expr(p);
      p.expect(Semicolon);
      p.close(m_stmt, CstKind::StmtLet)
    }
    KwLoop => {
      p.expect(KwLoop);
      let m_body = p.open_eat_trivia();
      if p.at(OpBrace) {
        do_body(p, m_body);
      } else {
        p.close(m_body, CstKind::ErrExpected(OpBrace));
      }
      p.close(m_stmt, CstKind::StmtLoop)
    }
    KwIf => {
      p.expect(KwIf);
      let m_condition = p.open_eat_trivia();
      if try_value_expr(p).is_none() {
        p.place_error(ErrExpectedIfCondition);
      }
      p.close(m_condition, CstKind::IfCondition);
      let m_body = p.open_eat_trivia();
      if p.at(OpBrace) {
        do_body(p, m_body);
      } else {
        p.close(m_body, CstKind::ErrExpected(OpBrace));
      }
      p.close(m_stmt, CstKind::StmtIf)
    }
    KwFor => {
      p.expect(KwFor);
      let m_expr = p.open_eat_trivia();
      if try_value_expr(p).is_some() {
        p.close(m_expr, CstKind::ValExpr);
      } else {
        p.close(m_expr, CstKind::ErrExpectedValueExpression);
      };
      p.expect(KwIn);
      let m_expr = p.open_eat_trivia();
      if try_value_expr(p).is_some() {
        p.close(m_expr, CstKind::ValExpr);
      } else {
        p.close(m_expr, CstKind::ErrExpectedValueExpression);
      };
      let m_body = p.open_eat_trivia();
      if p.at(OpBrace) {
        do_body(p, m_body);
      } else {
        p.close(m_body, CstKind::ErrExpected(OpBrace));
      }
      p.close(m_stmt, CstKind::StmtFor)
    }
    _ => {
      if try_value_expr(p).is_some() {
        p.eat_trivia();
        if p.at(Semicolon) {
          p.expect(Semicolon);
        }
        p.close(m_stmt, CstKind::StmtExpression)
      } else {
        p.advance();
        p.close(m_stmt, CstKind::ErrTodo)
      }
    }
  }
}

pub fn do_body(p: &mut CstParser, m_body: OpenMark) -> CloseMark {
  debug_assert_ne!(p.peek(), Whitespace);
  debug_assert_ne!(p.peek(), Comment);
  debug_assert_ne!(p.peek(), ErrEndOfFile);
  p.expect(OpBrace);
  loop {
    let m_stmt = p.open_eat_trivia();
    if p.at(ClBrace) {
      p.close(m_stmt, CstKind::StmtEmpty);
      p.expect(ClBrace);
      return p.close(m_body, CstKind::Body);
    }
    if p.at(ErrEndOfFile) {
      p.close(m_stmt, CstKind::StmtEmpty);
      return p.close(m_body, CstKind::ErrExpected(ClBrace));
    }
    do_stmt(p, m_stmt);
  }
}

pub fn do_func(p: &mut CstParser, m_fn: OpenMark) -> CloseMark {
  debug_assert!(p.at(KwFn));

  p.expect(KwFn);
  p.eat_trivia();
  p.expect(Ident);
  p.eat_trivia();
  let m_args = p.open();
  p.expect(OpParen);
  loop {
    p.eat_trivia();
    if p.at(ClParen) {
      break;
    }
    p.expect(Ident);
    p.eat_trivia();
    p.expect(Colon);
    p.eat_trivia();
    if try_type_expr(p).is_none() {
      let e = p.open();
      p.close(e, CstKind::ErrExpectedTypeExpression);
    }
    p.eat_trivia();
    if p.at(Comma) {
      p.expect(Comma);
    }
    p.eat_trivia();
  }
  p.expect(ClParen);
  p.close(m_args, CstKind::ArgumentList);
  p.eat_trivia();
  if p.at(Minus) {
    let m_ret_ty = p.open();
    p.expect(Minus);
    p.expect(GreaterThan);
    p.eat_trivia();
    if try_type_expr(p).is_none() {
      let e = p.open();
      p.close(e, CstKind::ErrExpectedTypeExpression);
    }
    p.close(m_ret_ty, CstKind::ReturnType);
  }
  let m_body = p.open_eat_trivia();
  do_body(p, m_body);

  p.close(m_fn, CstKind::Function)
}

static ITEM_KEYWORDS: &[TokenKind] =
  &[KwUse, KwStruct, KwBitbag, KwEnum, KwStatic, KwConst, KwFn];

pub fn do_item(p: &mut CstParser, m: OpenMark) -> CloseMark {
  debug_assert!(
    ITEM_KEYWORDS.contains(&p.peek()),
    "bad do_item: {:?}",
    p.peek()
  );
  match p.peek() {
    KwFn => do_func(p, m),
    _ => {
      p.advance();
      p.close(m, CstKind::ErrTodo)
    }
  }
}

/// Parse an entire module's content.
pub fn do_module(p: &mut CstParser) {
  let m_module = p.open();
  while p.peek() != ErrEndOfFile {
    let m_item = p.open_eat_trivia();
    if ITEM_KEYWORDS.contains(&p.peek()) {
      do_item(p, m_item);
    } else {
      while !ITEM_KEYWORDS.contains(&p.peek()) && p.has_more() {
        p.advance();
      }
      p.close(m_item, CstKind::ErrExpectedItemKeyword);
    }
  }
  p.close(m_module, CstKind::Module);
}
