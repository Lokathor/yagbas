#![allow(dead_code)]
//! Module for free functions that manipulate a [CstParser] to build a useful
//! [Cst]

use crate::cst::CstKind::{self, ErrExpectedIfCondition};
use crate::cst::operators::{
  BindDirection, InfixOperator, PostfixOperator, PrefixOperator,
};
use crate::cst::parser::{CloseMark, CstParser, OpenMark};
use crate::tokenizer::TokenKind::*;
use crate::tokenizer::{Token, TokenKind, tokenize};

static ITEM_KEYWORDS: &[TokenKind] =
  &[KwUse, KwStruct, KwBitbag, KwEnum, KwStatic, KwConst, KwFn, KwImpl];

/// Parse an entire module's content.
pub fn do_module(p: &mut CstParser) {
  let m_module = p.open();
  while p.peek() != ErrEndOfFile {
    let m_item = p.open_eat_trivia();
    if ITEM_KEYWORDS.contains(&p.peek()) {
      do_item(p, m_item);
    } else if p.peek() == ErrEndOfFile {
      p.abandon_subtree(m_item);
    } else {
      while !ITEM_KEYWORDS.contains(&p.peek()) && p.has_more() {
        p.advance();
      }
      p.close(m_item, CstKind::ErrExpectedItemKeyword);
    }
    while let Whitespace = p.peek() {
      p.advance();
    }
  }
  p.close(m_module, CstKind::Module);
}

/// Parse for one single item.
///
/// * `m_item` the mark for the tree holding this item
/// * **Debug Assert:** that the parser is already pointed at an item keyword.
fn do_item(p: &mut CstParser, m_item: OpenMark) -> CloseMark {
  debug_assert!(
    ITEM_KEYWORDS.contains(&p.peek()),
    "bad do_item: {:?}",
    p.peek()
  );
  match p.peek() {
    KwFn => do_func(p, m_item),
    KwStatic => do_static(p, m_item),
    KwConst => do_const(p, m_item),
    _ => {
      p.advance();
      p.close(m_item, CstKind::ErrTodo)
    }
  }
}

fn do_const(p: &mut CstParser, m_item: OpenMark) -> CloseMark {
  debug_assert_eq!(p.peek(), KwConst);
  p.expect(KwConst);
  p.eat_trivia();
  p.expect(Ident);
  p.eat_trivia();
  p.expect(Colon);
  p.eat_trivia();
  do_type_expr(p);
  p.eat_trivia();
  p.expect(Equal);
  p.eat_trivia();
  try_value_expr(p);
  p.eat_trivia();
  p.expect(Semicolon);
  p.close(m_item, CstKind::ItemConst)
}

fn do_static(p: &mut CstParser, m_item: OpenMark) -> CloseMark {
  debug_assert_eq!(p.peek(), KwStatic);
  p.expect(KwStatic);
  p.eat_trivia();
  match p.peek() {
    KwMmio => {
      p.expect(KwMmio);
      p.eat_trivia();
      p.expect(OpParen);
      p.eat_trivia();
      try_value_expr(p);
      p.eat_trivia();
      p.expect(ClParen);
      p.eat_trivia();
      p.expect(Ident);
      p.eat_trivia();
      p.expect(Colon);
      p.eat_trivia();
      do_type_expr(p);
      p.eat_trivia();
      p.expect(Semicolon);
      p.close(m_item, CstKind::ItemStaticMmio)
    }
    _ => {
      p.advance();
      p.close(m_item, CstKind::ErrTodo)
    }
  }
}

/// Parse one function definition
///
/// * `m_fn` the mark for this function
/// * **Debug Assert:** That the parser is pointed at the `fn` keyword.
fn do_func(p: &mut CstParser, m_fn: OpenMark) -> CloseMark {
  debug_assert!(p.at(KwFn));
  p.expect(KwFn);
  p.eat_trivia();
  p.expect(Ident);
  p.eat_trivia();

  if p.at(OpParen) {
    let m_arguments = p.open();
    do_function_arguments(p);
    p.close(m_arguments, CstKind::ArgumentList);
  } else {
    p.place_error(CstKind::ErrExpected(OpParen));
  }

  let m_return_ty = p.open_eat_trivia();
  if p.at(Minus) {
    p.expect(Minus);
    p.expect(GreaterThan);
    p.eat_trivia();
    do_type_expr(p);
  }
  p.close(m_return_ty, CstKind::ReturnType);

  let m_body = p.open_eat_trivia();
  do_body(p, m_body);

  p.close(m_fn, CstKind::ItemFunction)
}

fn do_function_arguments(p: &mut CstParser) {
  debug_assert_eq!(p.peek(), OpParen);
  p.expect(OpParen);
  let mut m_arg = p.open_eat_trivia();
  loop {
    if p.at(ClParen) {
      p.abandon_subtree(m_arg);
      p.advance();
      return;
    }
    p.expect(Ident);
    p.eat_trivia();
    p.expect(Colon);
    p.eat_trivia();
    do_type_expr(p);
    p.eat_trivia();
    if p.at(Comma) {
      p.close(m_arg, CstKind::FnCallArgument);
      p.expect(Comma);
      m_arg = p.open_eat_trivia();
    }
  }
}

fn do_type_expr(p: &mut CstParser) {
  debug_assert_ne!(p.peek(), Whitespace);
  debug_assert_ne!(p.peek(), Comment);
  let m_type_expr = p.open();
  match p.peek() {
    OpParen => {
      p.expect(OpParen);
      p.eat_trivia();
      p.expect(ClParen);
    }
    Ident => {
      p.expect(Ident);
      p.eat_trivia();
      let mut depth = 0;
      if p.at(LessThan) {
        p.advance();
        p.eat_trivia();
        depth += 1;
      }
      while depth > 0 {
        match p.peek() {
          LessThan => {
            p.advance();
            p.eat_trivia();
            depth += 1;
          }
          GreaterThan => {
            p.advance();
            p.eat_trivia();
            depth -= 1;
          }
          ErrEndOfFile => {
            p.place_error(CstKind::ErrUnbalancedAngleMarks);
            break;
          }
          _ => {
            p.advance();
          }
        }
      }
    }
    OpBracket => {
      p.expect(OpBracket);
      p.eat_trivia();
      do_type_expr(p);
      p.eat_trivia();
      p.expect(Semicolon);
      p.eat_trivia();
      try_value_expr(p);
      p.eat_trivia();
      p.expect(ClBracket);
    }
    _ => {
      if p.peek() != ErrEndOfFile {
        p.advance();
      }
      p.close(m_type_expr, CstKind::ErrExpectedTypeExpression);
      return;
    }
  }
  p.close(m_type_expr, CstKind::TypeExpr);
}

/// Parse a value expression, or `None` for no input consumed.
fn try_value_expr(p: &mut CstParser) -> Option<CloseMark> {
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
        p.close(m, CstKind::ValExpr)
      }
      OpParen => {
        let m = p.open();
        p.expect(OpParen);
        p.eat_trivia();
        try_value_expr(p);
        p.eat_trivia();
        p.expect(ClParen);
        p.close(m, CstKind::ValExpr)
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
            do_type_expr(p);
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
          // When we see an Ambiguious operator without parens we emit this
          // error and then just keep parsing as if it was a left leaning
          // operator so that the parsing completes not matter what.
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

fn do_stmt(p: &mut CstParser, m_stmt: OpenMark) -> CloseMark {
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
      let m_pattern = p.open_eat_trivia();
      p.expect(Ident);
      p.close(m_pattern, CstKind::Pattern);
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

fn do_body(p: &mut CstParser, m_body: OpenMark) -> CloseMark {
  debug_assert_ne!(p.peek(), Whitespace);
  debug_assert_ne!(p.peek(), Comment);
  debug_assert_ne!(p.peek(), ErrEndOfFile);
  p.expect(OpBrace);
  loop {
    let m_stmt = p.open_eat_trivia();
    if p.at(ClBrace) {
      p.abandon_subtree(m_stmt);
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
