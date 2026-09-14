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
          let peek = p.peek();
          if ITEM_KEYWORDS.contains(&peek) || peek == Comment {
            break;
          }
          p.advance();
        }
        p.close(m_item, CstKind::ErrCstKind);
      }
    }
  }
}

/// Eats one item.
///
/// Panics if `p.peek()` isn't an item keyword.
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
  let mut brace_depth = 0_isize;
  let mut braces = Vec::new();
  while p.has_more() {
    match p.peek() {
      OpBrace => {
        brace_depth += 1;
        braces.push(p.open());
      }
      ClBrace => {
        brace_depth -= 1;
        if let Some(mark) = braces.pop() {
          p.advance();
          p.close(mark, CstKind::BracketGroup);
          continue;
        }
      }
      _ if brace_depth == 0 => break,
      _ => (),
    }
    p.advance();
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
  while p.has_more() && p.peek() != ClBrace {
    p.advance();
  }
  p.expect(ClBrace);
  p.close(m_braces, CstKind::BraceGroup);
}

fn do_use(p: &mut CstParser<'_>) {
  p.expect(KwUse);
  while p.has_more() && p.peek() != Semicolon {
    p.advance();
  }
  p.advance();
}
