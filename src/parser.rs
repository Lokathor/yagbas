#![allow(unused)]

use std::cell::Cell;

use crate::tokenizer::{Token, TokenKind};

/// Concrete Syntax Tree
#[derive(Debug, Clone)]
pub struct Cst {
  pub kind: CstKind,
  pub elements: Vec<CstElem>,
}

/// Concrete Syntax Tree Kinds
///
/// There's one kind per "thing that can hold more things".
#[derive(Debug, Clone, Copy)]
pub enum CstKind {
  CstKindError,
  Module,
  Bitbag,
  Structure,
  Static,
  Constant,
  Function,
  ParamList,
  Param,
  TypeExpr,
  Block,
}

/// Concrete Syntax Tree Element, a single token or an entire sub-tree.
#[derive(Debug, Clone)]
pub enum CstElem {
  Token(Token),
  Tree(Cst),
}

#[derive(Debug, Clone, Copy)]
enum ParseEvent {
  Open(CstKind),
  Close,
  Advance,
}

#[derive(Debug, Clone, Copy)]
struct OpenMarker {
  index: usize,
}

#[derive(Debug, Clone)]
pub struct Parser {
  tokens: Vec<Token>,
  pos: usize,
  fuel: Cell<u32>,
  events: Vec<ParseEvent>,
}
impl Parser {
  /// Open up a new sub-tree and get the marker that will close it later.
  ///
  /// the sub-tree will be an error until it's closed.
  fn open(&mut self) -> OpenMarker {
    let mark = OpenMarker { index: self.events.len() };
    self.events.push(ParseEvent::Open(CstKind::CstKindError));
    mark
  }
  /// Close a given marker, and set it to the given kind.
  fn close(&mut self, m: OpenMarker, kind: CstKind) {
    self.events[m.index] = ParseEvent::Open(kind);
    self.events.push(ParseEvent::Close);
  }
  fn advance(&mut self) {
    assert!(!self.eof());
    self.fuel.set(256);
    self.events.push(ParseEvent::Advance);
    self.pos += 1;
  }
  fn eof(&self) -> bool {
    self.pos == self.tokens.len()
  }
  fn has_more(&self) -> bool {
    debug_assert!(self.pos <= self.tokens.len());
    self.pos < self.tokens.len()
  }
  fn nth(&self, lookahead: usize) -> TokenKind {
    if self.fuel.get() == 0 {
      panic!("stuck!")
    }
    self.fuel.set(self.fuel.get() - 1);
    self
      .tokens
      .get(self.pos + lookahead)
      .map_or(TokenKind::ErrEndOfFile, |tk| tk.kind)
  }
  fn at(&self, kind: TokenKind) -> bool {
    self.nth(0) == kind
  }
  fn eat(&mut self, kind: TokenKind) -> bool {
    if self.at(kind) {
      self.advance();
      true
    } else {
      false
    }
  }
  fn expect(&mut self, kind: TokenKind) {
    if self.eat(kind) {
      return;
    }
    eprintln!("Expected: {kind:?}");
  }
  fn advance_with_error(&mut self, error: &str) {
    let m = self.open();
    eprintln!("Error: {error}");
    self.advance();
    self.close(m, CstKind::CstKindError);
  }
  fn build_tree(self) -> Cst {
    let mut tokens = self.tokens.into_iter();
    let mut events = self.events;
    let mut stack = Vec::new();
    assert!(matches!(events.pop(), Some(ParseEvent::Close)));

    for event in events {
      match event {
        ParseEvent::Open(kind) => {
          stack.push(Cst { kind, elements: Vec::new() })
        }
        ParseEvent::Close => {
          let tree = stack.pop().unwrap();
          stack.last_mut().unwrap().elements.push(CstElem::Tree(tree));
        }
        ParseEvent::Advance => {
          let token = tokens.next().unwrap();
          stack.last_mut().unwrap().elements.push(CstElem::Token(token));
        }
      }
    }
    assert_eq!(stack.len(), 1);
    assert!(tokens.next().is_none());
    stack.pop().unwrap()
  }
}

fn do_module(p: &mut Parser) {
  let m = p.open();

  while p.has_more() {
    if p.at(TokenKind::KwFn) {
      do_function(p);
    } else {
      p.advance_with_error("Expected keyword `fn`.");
    }
  }

  p.close(m, CstKind::Module);
}

fn do_function(p: &mut Parser) {
  debug_assert!(p.at(TokenKind::KwFn));
  let m = p.open();

  p.expect(TokenKind::KwFn);
  p.expect(TokenKind::Ident);
  if p.at(TokenKind::OpParen) {
    do_param_list(p);
  }
  if p.eat(TokenKind::Minus) && p.eat(TokenKind::GreaterThan) {
    do_type_expr(p);
  }
  if p.at(TokenKind::OpBrace) {
    do_block(p);
  }

  p.close(m, CstKind::Function);
}

fn do_param_list(p: &mut Parser) {
  debug_assert!(p.at(TokenKind::OpParen));
  let m = p.open();

  p.expect(TokenKind::OpParen);
  while !p.at(TokenKind::ClParen) && p.has_more() {
    if p.at(TokenKind::Ident) {
      do_param(p);
    } else {
      break;
    }
  }
  p.expect(TokenKind::ClParen);

  p.close(m, CstKind::ParamList);
}

fn do_param(p: &mut Parser) {
  debug_assert!(p.at(TokenKind::Ident));
  let m = p.open();

  p.expect(TokenKind::Ident);
  p.expect(TokenKind::Colon);
  do_type_expr(p);
  if !p.at(TokenKind::ClParen) {
    p.expect(TokenKind::Comma);
  }

  p.close(m, CstKind::Param);
}

fn do_type_expr(p: &mut Parser) {
  let m = p.open();
  p.expect(TokenKind::Ident);
  p.close(m, CstKind::TypeExpr);
}

fn do_block(p: &mut Parser) {
  debug_assert!(p.at(TokenKind::OpBrace));
  let m = p.open();

  p.expect(TokenKind::OpBrace);
  while !p.at(TokenKind::ClBrace) && p.has_more() {
    match p.nth(0) {
      TokenKind::KwLet => todo!(),
      TokenKind::KwReturn => todo!(),
      _ => todo!(),
    }
  }

  p.close(m, CstKind::Block);
}
