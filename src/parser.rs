use std::cell::Cell;

use crate::tokenizer::{Token, TokenKind};

#[derive(Debug, Clone)]
pub struct SyntaxTree {
  pub kind: SyntaxTreeKind,
  pub elements: Vec<TreeElement>,
}

#[derive(Debug, Clone, Copy)]
pub enum SyntaxTreeKind {
  SyntaxTreeKindError,
  Module,
  Bitbag,
  Structure,
  Static,
  Constant,
  Function,
  // TODO: we need all sorts of syntax tree kinds. the exact list will be
  // discovered as we build out the parser itself.
}

#[derive(Debug, Clone)]
pub enum TreeElement {
  Token(Token),
  Tree(SyntaxTree),
}

#[derive(Debug, Clone, Copy)]
enum Event {
  Open(SyntaxTreeKind),
  Close,
  Advance,
}

#[derive(Debug, Clone, Copy)]
struct MarkOpened {
  index: usize,
}

#[derive(Debug, Clone)]
pub struct Parser {
  tokens: Vec<Token>,
  pos: usize,
  fuel: Cell<u32>,
  events: Vec<Event>,
}
impl Parser {
  fn open(&mut self) -> MarkOpened {
    let mark = MarkOpened { index: self.events.len() };
    self.events.push(Event::Open(SyntaxTreeKind::SyntaxTreeKindError));
    mark
  }
  fn close(&mut self, m: MarkOpened, kind: SyntaxTreeKind) {
    self.events[m.index] = Event::Open(kind);
    self.events.push(Event::Close);
  }
  fn advance(&mut self) {
    assert!(!self.eof());
    self.fuel.set(256);
    self.events.push(Event::Advance);
    self.pos += 1;
  }
  fn eof(&self) -> bool {
    self.pos == self.tokens.len()
  }
  fn nth(&self, lookahead: usize) -> TokenKind {
    if self.fuel.get() == 0 {
      panic!("stuck!")
    }
    self.fuel.set(self.fuel.get() - 1);
    self
      .tokens
      .get(self.pos + lookahead)
      .map_or(TokenKind::EndOfFile, |tk| tk.kind)
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
    self.close(m, SyntaxTreeKind::SyntaxTreeKindError);
  }
  fn build_tree(self) -> SyntaxTree {
    let mut tokens = self.tokens.into_iter();
    let mut events = self.events;
    let mut stack = Vec::new();
    assert!(matches!(events.pop(), Some(Event::Close)));

    for event in events {
      match event {
        Event::Open(kind) => {
          stack.push(SyntaxTree { kind, elements: Vec::new() })
        }
        Event::Close => {
          let tree = stack.pop().unwrap();
          stack.last_mut().unwrap().elements.push(TreeElement::Tree(tree));
        }
        Event::Advance => {
          let token = tokens.next().unwrap();
          stack.last_mut().unwrap().elements.push(TreeElement::Token(token));
        }
      }
    }
    assert_eq!(stack.len(), 1);
    assert!(tokens.next().is_none());
    stack.pop().unwrap()
  }
}
