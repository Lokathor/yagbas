//! Module for the core [CstParser] type that builds [Cst] values.
//!
//! The free functions to usefully manipulate the parser according to the
//! particulars of the Yagbas language are in [crate::cst::actions].

use core::range::Range;

use crate::{
  Span,
  tokenizer::{
    TokenKind::{self, Comment, Whitespace},
    tokenize,
  },
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
pub struct CstParser<'a> {
  /// The source that the tokens apply to, which is needed during CstElem
  /// construction.
  src: &'a str,
  /// We store the token components separately during the parsing because the
  /// parsing process never needs to look at the position value. By having a
  /// vector of *just* TokenKind values, we can keep 64 kind values in a cache
  /// line instead of just 12ish.
  token_kinds: Vec<TokenKind>,
  /// Not used during the actual parse process, just for the tree creation at
  /// the end.
  token_spans: Vec<Span>,
  /// Our current position within the input.
  pos: usize,
  /// The events that we've recorded so far. There will always be one `advance`
  /// per input token, as well as a starting `Open` and `Close` event, as well
  /// as all the other `Open` and `Close` events inserted by the tree structure.
  events: Vec<ParseEvent>,
}
impl<'a> CstParser<'a> {
  /// Makes a new parser.
  ///
  /// This pre-allocates the buffers used during parsing, so it's not totally
  /// free.
  pub fn new(src: &'a str) -> Self {
    // There will never be more tokens than the source length, so we can use it
    // as an approximation for how big the buffers need to be. Mild
    // over-allocation won't kill anything.
    let buffer_length = src.len();
    let mut token_kinds = Vec::with_capacity(buffer_length);
    let mut token_spans = Vec::with_capacity(buffer_length);
    let events = Vec::with_capacity(buffer_length);
    for Token { kind, span } in tokenize(src) {
      token_kinds.push(kind);
      token_spans.push(span);
    }
    Self { src, token_kinds, token_spans, pos: 0, events }
  }
  /// Open a new sub-tree
  pub fn open(&mut self) -> OpenMark {
    let mark = OpenMark { index: self.events.len() };
    self.events.push(ParseEvent::Open(CstKind::ErrCstKindDefault));
    mark
  }
  /// Open a sub-tree starting *before* the sub-tree that this close mark was
  /// for. So the newly opened sub-tree will automatically include the tree for
  /// this close mark.
  pub fn open_before(&mut self, m: CloseMark) -> OpenMark {
    let mark = OpenMark { index: m.index };
    self.events.insert(m.index, ParseEvent::Open(CstKind::ErrCstKindDefault));
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
  /// This makes certain kinds of parser looping a lot cleaner to write.
  ///
  /// This does a debug assert that there have only been `advance` events since
  /// the open mark was created.
  #[cfg_attr(debug_assertions, track_caller)]
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
  /// if the parser has more tokens.
  pub fn has_more(&self) -> bool {
    debug_assert!(self.pos <= self.token_kinds.len());
    self.pos < self.token_kinds.len()
  }
  /// check the next token kind without advancing.
  pub fn peek(&self) -> TokenKind {
    self.token_kinds.get(self.pos).copied().unwrap_or(TokenKind::ErrEndOfFile)
  }
  /// Peeks to check for `expected`, then advances, then returns if the expected
  /// kind was found or not.
  pub fn expect(&mut self, expected: TokenKind) -> bool {
    let k = self.peek();
    self.advance();
    k == expected
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
  pub fn build_tree(mut self, args: BuildTreeArgs) -> Cst {
    let mut token_kinds = self.token_kinds.iter().copied();
    let mut token_spans = self.token_spans.iter().copied();
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
          stack.last_mut().unwrap().elements.push(CstElem::SubTree(tree));
        }
        ParseEvent::Advance => {
          let kind = token_kinds.next().unwrap();
          let span = token_spans.next().unwrap();
          let elem = match kind {
            TokenKind::ErrEndOfFile
            | TokenKind::ErrDefault
            | TokenKind::ErrUnknownByte
            | TokenKind::ErrBlockCommentUnclosed
            | TokenKind::ErrBlockCommentExtraClose
            | TokenKind::ErrLitStrUnclosed
            | TokenKind::ErrLitRawStrUnclosed
            | TokenKind::ErrBadRawValue => {
              let bytes = self.src.as_bytes();
              CstElem::ErrorBytes(bytes[span.as_range()].to_vec(), Some(span))
            }
            TokenKind::Bang
            | TokenKind::DoubleQuote
            | TokenKind::Hash
            | TokenKind::Dollar
            | TokenKind::Percent
            | TokenKind::Ampersand
            | TokenKind::Quote
            | TokenKind::OpParen
            | TokenKind::ClParen
            | TokenKind::Star
            | TokenKind::Plus
            | TokenKind::Comma
            | TokenKind::Minus
            | TokenKind::Dot
            | TokenKind::Slash
            | TokenKind::Colon
            | TokenKind::Semicolon
            | TokenKind::LessThan
            | TokenKind::Equal
            | TokenKind::GreaterThan
            | TokenKind::Question
            | TokenKind::At
            | TokenKind::OpBracket
            | TokenKind::Backslash
            | TokenKind::ClBracket
            | TokenKind::Caret
            | TokenKind::Underscore
            | TokenKind::Backtick
            | TokenKind::OpBrace
            | TokenKind::Pipe
            | TokenKind::ClBrace
            | TokenKind::Tilde
            | TokenKind::ColonColon
            | TokenKind::EqualEqual
            | TokenKind::BangEqual
            | TokenKind::DotDot
            | TokenKind::DotDotEqual
            | TokenKind::PlusEqual
            | TokenKind::MinusEqual
            | TokenKind::StarEqual
            | TokenKind::SlashEqual
            | TokenKind::PercentEqual
            | TokenKind::AmpersandEqual
            | TokenKind::PipeEqual
            | TokenKind::CaretEqual
            | TokenKind::MinusGreater
            | TokenKind::KwAs
            | TokenKind::KwBitbag
            | TokenKind::KwBreak
            | TokenKind::KwConst
            | TokenKind::KwContinue
            | TokenKind::KwElse
            | TokenKind::KwEnum
            | TokenKind::KwFalse
            | TokenKind::KwFn
            | TokenKind::KwFor
            | TokenKind::KwIf
            | TokenKind::KwImpl
            | TokenKind::KwIn
            | TokenKind::KwLet
            | TokenKind::KwLoop
            | TokenKind::KwMatch
            | TokenKind::KwMmio
            | TokenKind::KwMod
            | TokenKind::KwMut
            | TokenKind::KwRam
            | TokenKind::KwReturn
            | TokenKind::KwRom
            | TokenKind::KwStruct
            | TokenKind::KwStatic
            | TokenKind::KwSuper
            | TokenKind::KwTrue
            | TokenKind::KwUse
            | TokenKind::KwWhile
            | TokenKind::KwVol => CstElem::FixedToken(kind, Some(span)),
            Whitespace => {
              if args.skip_trivial {
                continue;
              } else {
                CstElem::Whitespace(
                  self.src[span.as_range()].to_string(),
                  Some(span),
                )
              }
            }
            Comment => {
              if args.skip_trivial {
                continue;
              } else {
                CstElem::Comment(
                  self.src[span.as_range()].to_string(),
                  Some(span),
                )
              }
            }
            TokenKind::Ident => CstElem::Identifier(
              self.src[span.as_range()].to_string(),
              Some(span),
            ),
            TokenKind::LitNum => CstElem::LitNumber(
              self.src[span.as_range()].to_string(),
              Some(span),
            ),
            TokenKind::LitStr => CstElem::LitString(
              self.src[span.as_range()].to_string(),
              Some(span),
            ),
          };
          stack.last_mut().unwrap().elements.push(elem);
        }
      }
    }

    debug_assert_eq!(stack.len(), 1);
    debug_assert!(token_kinds.next().is_none(), "{:?}", self.token_kinds);
    stack.pop().unwrap()
  }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct BuildTreeArgs {
  /// Skip `Whitespace` and `Comment` elements when building trees.
  pub skip_trivial: bool,
}
