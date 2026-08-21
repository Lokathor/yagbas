#![allow(unused)]

use crate::r;
use crate::tokenizer::TokenKind::*;
use crate::tokenizer::{Token, TokenKind, tokenize};

use parser_core::*;
mod parser_core {
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

    // TODO: error logging

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
  ParenGroup,
  Identifier,
  LiteralNumber,
  InfixOperator,
  PrefixOperator,
  PostfixOperator,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CstElem {
  Token(Token),
  Tree(Cst),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BindDirection {
  Left,
  Right,
  Ambiguious,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OperatorKind {
  Path,
  FieldAccess,
  FnCall,
  ArrayIndex,
  Try,
  Negative,
  BitNot,
  Dereference,
  Reference,
  As,
  Mul,
  Div,
  Rem,
  Add,
  Sub,
  ShiftLeft,
  ShiftRight,
  BitAnd,
  BitXor,
  BitOr,
  CmpEq,
  CmpNe,
  CmpLt,
  CmpGt,
  CmpLe,
  CmpGe,
  ConditionalAnd,
  ConditionalOr,
  RangeExclusive,
  RangeInclusive,
  Assign,
  AddAssign,
  SubAssign,
  MulAssign,
  DivAssign,
  RemAssign,
  BitAndAssign,
  BitOrAssign,
  BitXorAssign,
  ShiftLeftAssign,
  ShiftRightAssign,
  Return,
  Break,
}
impl OperatorKind {
  pub const fn binding(self) -> (u8, BindDirection) {
    match self {
      OperatorKind::Return | OperatorKind::Break => {
        (2, BindDirection::Ambiguious)
      }
      OperatorKind::Assign
      | OperatorKind::AddAssign
      | OperatorKind::SubAssign
      | OperatorKind::MulAssign
      | OperatorKind::DivAssign
      | OperatorKind::RemAssign
      | OperatorKind::BitAndAssign
      | OperatorKind::BitOrAssign
      | OperatorKind::BitXorAssign
      | OperatorKind::ShiftLeftAssign
      | OperatorKind::ShiftRightAssign => (4, BindDirection::Right),
      OperatorKind::RangeExclusive | OperatorKind::RangeInclusive => {
        (6, BindDirection::Ambiguious)
      }
      OperatorKind::ConditionalOr => (8, BindDirection::Left),
      OperatorKind::ConditionalAnd => (10, BindDirection::Left),
      OperatorKind::CmpEq
      | OperatorKind::CmpNe
      | OperatorKind::CmpLt
      | OperatorKind::CmpGt
      | OperatorKind::CmpLe
      | OperatorKind::CmpGe => (12, BindDirection::Ambiguious),
      OperatorKind::BitOr => (14, BindDirection::Left),
      OperatorKind::BitXor => (16, BindDirection::Left),
      OperatorKind::BitAnd => (18, BindDirection::Left),
      OperatorKind::ShiftLeft | OperatorKind::ShiftRight => {
        (20, BindDirection::Left)
      }
      OperatorKind::Add | OperatorKind::Sub => (22, BindDirection::Left),
      OperatorKind::Mul | OperatorKind::Div | OperatorKind::Rem => {
        (24, BindDirection::Left)
      }
      OperatorKind::As => (26, BindDirection::Left),
      OperatorKind::Negative
      | OperatorKind::BitNot
      | OperatorKind::Dereference
      | OperatorKind::Reference => (28, BindDirection::Left),
      OperatorKind::Try => (30, BindDirection::Left),
      OperatorKind::FnCall | OperatorKind::ArrayIndex => {
        (32, BindDirection::Left)
      }
      OperatorKind::FieldAccess => (34, BindDirection::Left),
      OperatorKind::Path => (36, BindDirection::Left),
    }
  }
}

/// Tries to get a **prefix** operator, or `None` and no input was consumed.
fn try_prefix_operator(p: &mut Parser) -> Option<OperatorKind> {
  let k = match p.peek() {
    Minus => OperatorKind::Negative,
    Bang => OperatorKind::BitNot,
    Star => OperatorKind::Dereference,
    Ampersand => OperatorKind::Reference,
    KwReturn => OperatorKind::Return,
    KwBreak => OperatorKind::Break,
    _ => return None,
  };
  p.advance();
  Some(k)
}
#[test]
fn test_try_prefix_operator() {
  let mut p = Parser::new(tokenize("-").collect());
  assert_eq!(try_prefix_operator(&mut p), Some(OperatorKind::Negative));
  let mut p = Parser::new(tokenize("!").collect());
  assert_eq!(try_prefix_operator(&mut p), Some(OperatorKind::BitNot));
  let mut p = Parser::new(tokenize("*").collect());
  assert_eq!(try_prefix_operator(&mut p), Some(OperatorKind::Dereference));
  let mut p = Parser::new(tokenize("return").collect());
  assert_eq!(try_prefix_operator(&mut p), Some(OperatorKind::Return));
  let mut p = Parser::new(tokenize("break").collect());
  assert_eq!(try_prefix_operator(&mut p), Some(OperatorKind::Break));
}

/// Tries to get an **infix** operator, or `None` and no input was consumed.
fn try_infix_operator(p: &mut Parser) -> Option<OperatorKind> {
  let k = match p.peek() {
    ColonColon => OperatorKind::Path,
    Dot => OperatorKind::FieldAccess,
    Star => OperatorKind::Mul,
    Slash => OperatorKind::Div,
    Percent => OperatorKind::Rem,
    Plus => OperatorKind::Add,
    Minus => OperatorKind::Sub,
    LessThan => {
      p.advance();
      return Some(match p.peek() {
        LessThan => {
          p.advance();
          return Some(match p.peek() {
            Equal => {
              p.advance();
              OperatorKind::ShiftLeftAssign
            }
            _ => OperatorKind::ShiftLeft,
          });
        }
        Equal => {
          p.advance();
          OperatorKind::CmpLe
        }
        _ => OperatorKind::CmpLt,
      });
    }
    GreaterThan => {
      p.advance();
      return Some(match p.peek() {
        GreaterThan => {
          p.advance();
          return Some(match p.peek() {
            Equal => {
              p.advance();
              OperatorKind::ShiftRightAssign
            }
            _ => OperatorKind::ShiftRight,
          });
        }
        Equal => {
          p.advance();
          OperatorKind::CmpGe
        }
        _ => OperatorKind::CmpGt,
      });
    }
    Ampersand => {
      p.advance();
      return Some(match p.peek() {
        Ampersand => {
          p.advance();
          OperatorKind::ConditionalAnd
        }
        _ => OperatorKind::BitAnd,
      });
    }
    AmpersandEqual => OperatorKind::BitAndAssign,
    Pipe => {
      p.advance();
      return Some(match p.peek() {
        Pipe => {
          p.advance();
          OperatorKind::ConditionalOr
        }
        _ => OperatorKind::BitOr,
      });
    }
    PipeEqual => OperatorKind::BitOrAssign,
    Caret => OperatorKind::BitXor,
    CaretEqual => OperatorKind::BitXorAssign,
    Equal => OperatorKind::Assign,
    EqualEqual => OperatorKind::CmpEq,
    BangEqual => OperatorKind::CmpNe,
    DotDot => OperatorKind::RangeExclusive,
    DotDotEqual => OperatorKind::RangeInclusive,
    PlusEqual => OperatorKind::AddAssign,
    MinusEqual => OperatorKind::SubAssign,
    StarEqual => OperatorKind::MulAssign,
    SlashEqual => OperatorKind::DivAssign,
    PercentEqual => OperatorKind::RemAssign,
    AmpersandEqual => OperatorKind::BitAndAssign,
    PipeEqual => OperatorKind::BitOrAssign,
    CaretEqual => OperatorKind::BitXorAssign,
    _ => return None,
  };
  p.advance();
  Some(k)
}
#[test]
fn test_try_infix_operator() {
  let mut p = Parser::new(tokenize("::").collect());
  assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::Path));
  let mut p = Parser::new(tokenize(".").collect());
  assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::FieldAccess));
  let mut p = Parser::new(tokenize("*").collect());
  assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::Mul));
  let mut p = Parser::new(tokenize("/").collect());
  assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::Div));
  let mut p = Parser::new(tokenize("%").collect());
  assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::Rem));
  let mut p = Parser::new(tokenize("+").collect());
  assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::Add));
  let mut p = Parser::new(tokenize("-").collect());
  assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::Sub));
  let mut p = Parser::new(tokenize("<<").collect());
  assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::ShiftLeft));
  let mut p = Parser::new(tokenize(">>").collect());
  assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::ShiftRight));
  let mut p = Parser::new(tokenize("&").collect());
  assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::BitAnd));
  let mut p = Parser::new(tokenize("^").collect());
  assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::BitXor));
  let mut p = Parser::new(tokenize("|").collect());
  assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::BitOr));
  let mut p = Parser::new(tokenize("==").collect());
  assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::CmpEq));
  let mut p = Parser::new(tokenize("!=").collect());
  assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::CmpNe));
  let mut p = Parser::new(tokenize("<").collect());
  assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::CmpLt));
  let mut p = Parser::new(tokenize(">").collect());
  assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::CmpGt));
  let mut p = Parser::new(tokenize("<=").collect());
  assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::CmpLe));
  let mut p = Parser::new(tokenize(">=").collect());
  assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::CmpGe));
  let mut p = Parser::new(tokenize("&&").collect());
  assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::ConditionalAnd));
  let mut p = Parser::new(tokenize("||").collect());
  assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::ConditionalOr));
  let mut p = Parser::new(tokenize("..").collect());
  assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::RangeExclusive));
  let mut p = Parser::new(tokenize("..=").collect());
  assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::RangeInclusive));
  let mut p = Parser::new(tokenize("=").collect());
  assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::Assign));
  let mut p = Parser::new(tokenize("+=").collect());
  assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::AddAssign));
  let mut p = Parser::new(tokenize("-=").collect());
  assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::SubAssign));
  let mut p = Parser::new(tokenize("*=").collect());
  assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::MulAssign));
  let mut p = Parser::new(tokenize("/=").collect());
  assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::DivAssign));
  let mut p = Parser::new(tokenize("%=").collect());
  assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::RemAssign));
  let mut p = Parser::new(tokenize("&=").collect());
  assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::BitAndAssign));
  let mut p = Parser::new(tokenize("|=").collect());
  assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::BitOrAssign));
  let mut p = Parser::new(tokenize("^=").collect());
  assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::BitXorAssign));
  let mut p = Parser::new(tokenize("<<=").collect());
  assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::ShiftLeftAssign));
  let mut p = Parser::new(tokenize(">>=").collect());
  assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::ShiftRightAssign));
}

/// Tries to get a **postfix** operator, or `None` and no input was consumed.
fn try_postfix_operator(p: &mut Parser) -> Option<OperatorKind> {
  let k = match p.peek() {
    OpParen => OperatorKind::FnCall,
    OpBracket => OperatorKind::ArrayIndex,
    Question => OperatorKind::Try,
    KwAs => OperatorKind::As,
    _ => return None,
  };
  p.advance();
  Some(k)
}
#[test]
fn test_try_postfix_operator() {
  let mut p = Parser::new(tokenize("(").collect());
  assert_eq!(try_postfix_operator(&mut p), Some(OperatorKind::FnCall));
  let mut p = Parser::new(tokenize("[").collect());
  assert_eq!(try_postfix_operator(&mut p), Some(OperatorKind::ArrayIndex));
  let mut p = Parser::new(tokenize("as").collect());
  assert_eq!(try_postfix_operator(&mut p), Some(OperatorKind::As));
}
