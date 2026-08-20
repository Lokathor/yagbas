#![allow(unused)]

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

mod parser_core {
  use super::*;
  use core::cell::Cell;

  #[derive(Debug, Clone, Copy)]
  enum ParseEvent {
    Open(CstKind),
    Close,
    Advance,
  }

  #[derive(Debug, Clone, Copy)]
  pub struct OpenMark {
    index: usize,
  }
  #[derive(Debug, Clone, Copy)]
  pub struct CloseMark {
    index: usize,
  }

  #[derive(Debug, Clone)]
  pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    events: Vec<ParseEvent>,
  }
  impl Parser {
    pub fn open(&mut self) -> OpenMark {
      let mark = OpenMark { index: self.events.len() };
      self.events.push(ParseEvent::Open(CstKind::CstKindError));
      mark
    }
    pub fn close(&mut self, m: OpenMark, kind: CstKind) -> CloseMark {
      self.events[m.index] = ParseEvent::Open(kind);
      self.events.push(ParseEvent::Close);
      CloseMark { index: m.index }
    }
    pub fn open_before(&mut self, m: CloseMark) -> OpenMark {
      let mark = OpenMark { index: m.index };
      self.events.insert(m.index, ParseEvent::Open(CstKind::CstKindError));
      mark
    }
    pub fn advance(&mut self) {
      debug_assert!(self.has_more());
      self.events.push(ParseEvent::Advance);
      self.pos += 1;
    }
    pub fn has_more(&self) -> bool {
      debug_assert!(self.pos <= self.tokens.len());
      self.pos < self.tokens.len()
    }
    pub fn at(&self, kind: TokenKind) -> bool {
      self.tokens.get(self.pos).map_or(TokenKind::ErrEndOfFile, |tk| tk.kind)
        == kind
    }

    pub fn build_tree(self) -> Cst {
      let mut tokens = self.tokens.into_iter();
      let mut events = self.events;
      let mut stack = Vec::new();

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

      debug_assert!(tokens.next().is_none());
      stack.pop().unwrap()
    }
  }
}
