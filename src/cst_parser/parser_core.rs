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
  pub fn expect(&mut self, kind: TokenKind) {
    if self.at(kind) {
      self.advance();
    } else {
      self.advance();
      // TODO: real error logging
      eprintln!("Expected {kind:?}");
    }
  }
  pub fn advance_with_error(&mut self, error: &str) {
    let m = self.open();
    // TODO: real error logging
    eprintln!("Error Message: {error}");
    self.advance();
    self.close(m, CstKind::ErrGeneric);
  }

  // TODO: method to eat whitespace/comments if any

  pub fn build_tree(mut self) -> Cst {
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
    stack.pop().unwrap()
  }
}
