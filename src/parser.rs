#![allow(unused)]

use std::cell::Cell;

use crate::tokenizer::{Token, TokenKind};

/// Concrete Syntax Tree
#[derive(Debug, Clone)]
pub struct Cst {
  pub kind: CstKind,
  pub elements: Vec<CstElem>,
}

#[derive(Debug, Clone, Copy)]
pub enum CstKind {
  CstKindError,
  //
  ValExpr,
  LiteralInt,
  LiteralFraction,
  Ident,
  Add,
  Sub,
  Mul,
  Div,
  Rem,
  Neg,
  BitAnd,
  BitOr,
  BitXor,
  Not,
  ShiftLeft,
  ShiftRight,
  FieldAccess,
  Index,
  FnCall,
  Reference,
  Dereference,
  Range,
  RangeFrom,
  RangeFull,
  RangeInclusive,
  RangeTo,
  RangeToInclusive,
}

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
#[derive(Debug, Clone, Copy)]
struct CloseMarker {
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
  fn open(&mut self) -> OpenMarker {
    let mark = OpenMarker { index: self.events.len() };
    self.events.push(ParseEvent::Open(CstKind::CstKindError));
    mark
  }
  fn close(&mut self, m: OpenMarker, kind: CstKind) -> CloseMarker {
    self.events[m.index] = ParseEvent::Open(kind);
    self.events.push(ParseEvent::Close);
    CloseMarker { index: m.index }
  }
  fn open_before(&mut self, m: CloseMarker) -> OpenMarker {
    let mark = OpenMarker { index: m.index };
    self.events.insert(m.index, ParseEvent::Open(CstKind::CstKindError));
    mark
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
