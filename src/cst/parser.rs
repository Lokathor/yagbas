use core::range::Range;

use crate::tokenizer::{TokenKind, tokenize};

use super::*;

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
pub struct CstParser {
  token_kinds: Vec<TokenKind>,
  token_positions: Vec<u32>,
  pos: usize,
  events: Vec<ParseEvent>,
}
impl CstParser {
  pub fn new(src: &str) -> Self {
    let mut token_kinds = Vec::with_capacity(src.len());
    let mut token_positions = Vec::with_capacity(src.len());
    for Token { kind, position } in tokenize(src) {
      token_kinds.push(kind);
      token_positions.push(position);
    }
    Self {
      token_kinds,
      token_positions,
      pos: 0,
      events: Vec::new(),
    }
  }
  pub fn open(&mut self) -> OpenMark {
    let mark = OpenMark { index: self.events.len() };
    self.events.push(ParseEvent::Open(CstKind::ErrNoTreeKindSet));
    mark
  }
  pub fn close(&mut self, m: OpenMark, kind: CstKind) -> CloseMark {
    self.events[m.index] = ParseEvent::Open(kind);
    self.events.push(ParseEvent::Close);
    CloseMark { index: m.index }
  }
  pub fn open_before(&mut self, m: CloseMark) -> OpenMark {
    let mark = OpenMark { index: m.index };
    self.events.insert(m.index, ParseEvent::Open(CstKind::ErrNoTreeKindSet));
    mark
  }
  #[cfg_attr(debug_assertions, track_caller)]
  pub fn advance(&mut self) {
    debug_assert!(self.has_more());
    self.events.push(ParseEvent::Advance);
    self.pos += 1;
  }

  pub fn has_more(&self) -> bool {
    debug_assert!(self.pos <= self.token_kinds.len());
    self.pos < self.token_kinds.len()
  }
  pub fn peek(&self) -> TokenKind {
    self.token_kinds.get(self.pos).copied().unwrap_or(TokenKind::ErrEndOfFile)
  }
  pub fn at(&self, kind: TokenKind) -> bool {
    self.peek() == kind
  }
  pub fn expect(&mut self, expected: TokenKind) {
    if !self.at(expected) {
      let e = self.open();
      self.close(e, CstKind::ErrExpected(expected) );
    } else {
      self.advance();
    }
  }
  pub fn advance_over_whitespace_and_comments(&mut self) {
    while let TokenKind::Whitespace | TokenKind::Comment = self.peek() {
      self.advance();
    }
  }
  /// An iterator over the tokens still waiting to be parsed.
  ///
  /// This lets you peek forward as much as you need before actually consuming
  /// anything.
  pub fn tokens_tail(&self) -> impl Iterator<Item = TokenKind> + Clone + '_ {
    debug_assert!(self.pos <= self.token_kinds.len());
    self.token_kinds[self.pos..].iter().copied()
  }

  pub fn build_tree(mut self) -> Cst {
    let mut token_kinds = self.token_kinds.iter().copied();
    let mut token_positions = self.token_positions.iter().copied();
    let mut stack = Vec::new();

    // remove the last close event so that we can pop the stack's final value
    // and return it at the end of the method.
    let last_event = self.events.pop();
    debug_assert!(
      matches!(last_event, Some(ParseEvent::Close)),
      "{last_event:?}"
    );

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
          let token = Token {
            kind: token_kinds.next().unwrap(),
            position: token_positions.next().unwrap(),
          };
          stack.last_mut().unwrap().elements.push(CstElem::Token(token));
        }
      }
    }

    debug_assert_eq!(stack.len(), 1);
    debug_assert!(token_kinds.next().is_none(), "{:?}", self.token_kinds);
    stack.pop().unwrap()
  }
}
