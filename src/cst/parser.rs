//! Module for the core [CstParser] type that builds [Cst] values.
//!
//! The free functions to usefully manipulate the parser according to the
//! particulars of the Yagbas language are in [crate::cst::actions].

use core::range::Range;

use crate::tokenizer::{
  TokenKind::{self, Comment, Whitespace},
  tokenize,
};

use super::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ParseEvent {
  Open(CstKind),
  Close,
  Advance,
}

/// Mark for the opening of a sub-tree
#[derive(Debug, Clone, Copy)]
pub struct OpenMark {
  index: usize,
}

/// Mark for the closing of a sub-tree
#[derive(Debug, Clone, Copy)]
pub struct CloseMark {
  index: usize,
}

/// Data for parsing a CST from a series of tokens.
#[derive(Debug, Clone)]
pub struct CstParser {
  /// We store the token components separately during the parsing because the
  /// parsing process never needs to look at the position value. By having a
  /// vector of *just* TokenKind values, we can keep 64 kind values in a cache
  /// line instead of just 12ish.
  token_kinds: Vec<TokenKind>,
  /// Not used during the actual parse process, just for the tree creation at
  /// the end.
  token_positions: Vec<u32>,
  /// Our current position within the input.
  pos: usize,
  /// The events that we've recorded so far. There will always be one `advance`
  /// per input token, as well as a starting `Open` and `Close` event, as well
  /// as all the other `Open` and `Close` events inserted by the tree structure.
  events: Vec<ParseEvent>,
}
impl CstParser {
  /// Makes a new parser.
  ///
  /// This pre-allocates the buffers used during parsing, so it's not totally
  /// free.
  pub fn new(src: &str) -> Self {
    // There will never be more tokens than the source length, so we can use it
    // as an approximation for how big the buffers need to be. Mild
    // over-allocation won't kill anything.
    let buffer_length = src.len();
    let mut token_kinds = Vec::with_capacity(buffer_length);
    let mut token_positions = Vec::with_capacity(buffer_length);
    let events = Vec::with_capacity(buffer_length);
    for Token { kind, position } in tokenize(src) {
      token_kinds.push(kind);
      token_positions.push(position);
    }
    Self { token_kinds, token_positions, pos: 0, events }
  }
  /// Open a new sub-tree
  pub fn open(&mut self) -> OpenMark {
    let mark = OpenMark { index: self.events.len() };
    self.events.push(ParseEvent::Open(CstKind::ErrNoTreeKindSet));
    mark
  }
  /// Open a sub-tree starting *before* the sub-tree that this close mark was
  /// for. So the newly opened sub-tree will automatically include the tree for
  /// this close mark.
  pub fn open_before(&mut self, m: CloseMark) -> OpenMark {
    let mark = OpenMark { index: m.index };
    self.events.insert(m.index, ParseEvent::Open(CstKind::ErrNoTreeKindSet));
    mark
  }
  /// Close a sub-tree and assign it a kind.
  ///
  /// You frequently enough don't know the kind of a tree until it's done
  /// processing, so you label the tree kind on closing.
  pub fn close(&mut self, m: OpenMark, kind: CstKind) -> CloseMark {
    self.events[m.index] = ParseEvent::Open(kind);
    self.events.push(ParseEvent::Close);
    CloseMark { index: m.index }
  }
  /// Abandon creation of the sub-tree for this open mark.
  ///
  /// This does a debug assert that there have only been `advance` events since
  /// the open mark was created.
  ///
  /// This makes certain kinds of parser looping a lot cleaner to write.
  pub fn abandon_subtree(&mut self, m: OpenMark) {
    for event in &self.events[(m.index + 1)..] {
      debug_assert_eq!(*event, ParseEvent::Advance);
    }
    self.events.remove(m.index);
  }
  /// Advance the parser over the current token kind.
  #[cfg_attr(debug_assertions, track_caller)]
  pub fn advance(&mut self) {
    debug_assert!(self.has_more());
    self.events.push(ParseEvent::Advance);
    self.pos += 1;
  }
  /// [Self::open] and [Self::eat_trivia] combined.
  pub fn open_eat_trivia(&mut self) -> OpenMark {
    let m_out = self.open();
    self.eat_trivia();
    m_out
  }
  /// Place an error message (but this doesn't affect the token position).
  pub fn place_error(&mut self, kind: CstKind) -> CloseMark {
    let m = self.open();
    self.close(m, kind)
  }
  /// if the parser has more tokens.
  pub fn has_more(&self) -> bool {
    debug_assert!(self.pos <= self.token_kinds.len());
    self.pos < self.token_kinds.len()
  }
  /// check the next token kind without advancing.
  pub fn peek(&self) -> TokenKind {
    self.token_kinds.get(self.pos).copied().unwrap_or(TokenKind::ErrEndOfFile)
  }
  /// if the parser is at a specific kind.
  pub fn at(&self, kind: TokenKind) -> bool {
    self.peek() == kind
  }
  /// Same as `advance` when the expected kind is the next kind.
  ///
  /// otherwise, the next token is placed into an error sub-tree during
  /// advancing.
  pub fn expect(&mut self, expected: TokenKind) -> bool {
    if !self.at(expected) {
      let e = self.open();
      self.advance();
      self.close(e, CstKind::ErrExpected(expected));
      false
    } else {
      self.advance();
      true
    }
  }
  /// `advance` over all [TokenKind::Whitespace] and [TokenKind::Comment] so
  /// that something "real" is the next kind.
  pub fn eat_trivia(&mut self) {
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

  /// Finish all parsing and process the event list into a [Cst].
  ///
  /// * All `open` events must have a matching `close` before attempting to create a tree.
  /// * All `advance` events must be inside of a tree.
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
