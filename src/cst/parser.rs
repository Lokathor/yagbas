use core::range::Range;

use crate::tokenizer::TokenKind;

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

#[derive(Debug, Clone, Copy)]
pub enum CstParserErrorKind {
  UnexpectedToken { expected: TokenKind, actual: Token },
  ExpectedFunctionArgument,
}

#[derive(Debug, Clone)]
pub struct CstParser {
  tokens: Vec<Token>,
  pos: usize,
  events: Vec<ParseEvent>,
  errors: Vec<CstParserErrorKind>,
}
impl CstParser {
  pub fn new(tokens: Vec<Token>) -> Self {
    Self { tokens, pos: 0, events: Vec::new(), errors: Vec::new() }
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
  pub fn expect(&mut self, expected: TokenKind) {
    if !self.at(expected) {
      self.errors.push(CstParserErrorKind::UnexpectedToken {
        expected,
        actual: self.tokens.get(self.pos).copied().unwrap_or(Token {
          kind: TokenKind::ErrEndOfFile,
          position: u32::MAX,
        }),
      });
    }
    self.advance();
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
  pub fn tokens_tail(&self) -> impl Iterator<Item = Token> + Clone + '_ {
    debug_assert!(self.pos <= self.tokens.len());
    self.tokens[self.pos..].iter().copied()
  }

  pub fn build_tree(mut self) -> (Cst, Vec<CstParserErrorKind>) {
    let mut tokens = self.tokens.iter().copied();
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
          let token = tokens.next().unwrap();
          stack.last_mut().unwrap().elements.push(CstElem::Token(token));
        }
      }
    }

    debug_assert_eq!(stack.len(), 1);
    debug_assert!(tokens.next().is_none(), "{:?}", self.tokens);
    (stack.pop().unwrap(), self.errors)
  }
}
