#![allow(unused)]

use crate::r;
use crate::tokenizer::TokenKind::*;
use crate::tokenizer::{Token, TokenKind, tokenize};
use CstKind::*;

use parser_core::*;
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
    pub fn new(tokens: Vec<Token>) -> Self {
      Self { tokens, pos: 0, events: Vec::new() }
    }
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
    pub fn peek(&self) -> TokenKind {
      if let Some(tk) = self.tokens.get(self.pos) {
        tk.kind
      } else {
        TokenKind::ErrEndOfFile
      }
    }
    pub fn at(&self, kind: TokenKind) -> bool {
      self.peek() == kind
    }

    pub fn build_tree(mut self) -> Cst {
      let mut tokens = self.tokens.iter().copied();
      let mut stack = Vec::new();

      // remove the last close event so that we can pop the stack's final value
      // and return it at the end of the method.
      let last_event = self.events.pop();
      debug_assert!(matches!(last_event, Some(ParseEvent::Close)));

      for event in self.events {
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

      debug_assert_eq!(stack.len(), 1);
      debug_assert!(tokens.next().is_none());
      stack.pop().unwrap()
    }
  }
}

/// Concrete Syntax Tree
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Cst {
  pub kind: CstKind,
  pub elements: Vec<CstElem>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CstKind {
  CstKindError,
  //
  ValExpr,
  LiteralNumber,
  LiteralBool,
  Identifier,
  ParenGroup,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CstElem {
  Token(Token),
  Tree(Cst),
}

fn expr_p(p: &mut Parser) {
  expr_atom_p(p);
}

fn expr_atom_p(p: &mut Parser) {
  debug_assert_ne!(p.peek(), Whitespace);
  let m = p.open();
  let kind = match p.peek() {
    LitNum => {
      p.advance();
      LiteralNumber
    }
    KwTrue | KwFalse => {
      p.advance();
      LiteralBool
    }
    Ident => {
      p.advance();
      Identifier
    }
    OpParen => {
      p.advance();
      if p.at(Whitespace) {
        p.advance();
        debug_assert_ne!(p.peek(), Whitespace);
      }
      expr_p(p);
      if p.at(Whitespace) {
        p.advance();
        debug_assert_ne!(p.peek(), Whitespace);
      }
      p.advance(); // TODO: assert ClParen
      ParenGroup
    }
    _ => todo!(),
  };
  p.close(m, kind);
}

#[test]
fn test_expr_atom() {
  let s = "1";
  let mut p = Parser::new(tokenize(s).collect());
  expr_p(&mut p);
  let cst = p.build_tree();
  assert_eq!(cst.kind, LiteralNumber);
  assert_eq!(cst.elements.len(), 1);

  let s = "true";
  let mut p = Parser::new(tokenize(s).collect());
  expr_p(&mut p);
  let cst = p.build_tree();
  assert_eq!(cst.kind, LiteralBool);
  assert_eq!(cst.elements.len(), 1);

  let s = "x";
  let mut p = Parser::new(tokenize(s).collect());
  expr_p(&mut p);
  let cst = p.build_tree();
  assert_eq!(cst.kind, Identifier);
  assert_eq!(cst.elements.len(), 1);

  let s = "(1)";
  let mut p = Parser::new(tokenize(s).collect());
  expr_p(&mut p);
  let cst = p.build_tree();
  assert_eq!(cst.kind, ParenGroup);
  assert_eq!(cst.elements.len(), 3);

  let s = "( true )";
  let mut p = Parser::new(tokenize(s).collect());
  expr_p(&mut p);
  let cst = p.build_tree();
  assert_eq!(cst.kind, ParenGroup);
  assert_eq!(cst.elements.len(), 5);
}
