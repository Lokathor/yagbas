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

    // TODO: method to eat whitespace/comments if any

    // TODO: error logging

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
}

/// Concrete Syntax Tree
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Cst {
  pub kind: CstKind,
  pub elements: Vec<CstElem>,
}
impl Cst {
  pub fn pretty_debug(&self) -> String {
    let mut buffer = String::new();
    self.pretty_debug_rec(0, &mut buffer);
    buffer
  }
  fn pretty_debug_rec(&self, indents: usize, buffer: &mut String) {
    use core::fmt::Write;
    for _ in 0..indents {
      write!(buffer, " ").ok();
    }
    writeln!(buffer, "{:?} {{", self.kind).ok();
    for element in &self.elements {
      match element {
        CstElem::Token(Token { kind, span }) => {
          for _ in 0..(indents + 2) {
            write!(buffer, " ").ok();
          }
          writeln!(buffer, "{kind:?} @({span:?})").ok();
        }
        CstElem::Tree(cst) => {
          cst.pretty_debug_rec(indents + 2, buffer);
        }
      }
    }
    for _ in 0..indents {
      write!(buffer, " ").ok();
    }
    writeln!(buffer, "}}").ok();
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CstKind {
  ErrNoTreeKindSet,
  ErrExpectedValueExpression,
  //
  ValExpr,
  AtomicValue,
  ParenGroup,
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
      OperatorKind::As => (26, BindDirection::Ambiguious),
      OperatorKind::Negative
      | OperatorKind::BitNot
      | OperatorKind::Dereference
      | OperatorKind::Reference => (28, BindDirection::Ambiguious),
      OperatorKind::Try => (30, BindDirection::Ambiguious),
      OperatorKind::FnCall | OperatorKind::ArrayIndex => {
        (32, BindDirection::Ambiguious)
      }
      OperatorKind::FieldAccess => (34, BindDirection::Left),
      OperatorKind::Path => (36, BindDirection::Left),
    }
  }
}

/// Tries to get a **prefix** operator, or `None` and no input was consumed.
fn try_prefix_operator(p: &mut Parser) -> Option<OperatorKind> {
  debug_assert_ne!(p.peek(), Whitespace);
  debug_assert_ne!(p.peek(), Comment);
  let k = match p.peek() {
    Minus => OperatorKind::Negative,
    Bang => OperatorKind::BitNot,
    Star => OperatorKind::Dereference,
    Ampersand => OperatorKind::Reference,
    KwReturn => OperatorKind::Return,
    KwBreak => OperatorKind::Break,
    _ => return None,
  };
  debug_assert_eq!(k.binding().1, BindDirection::Ambiguious);
  let m = p.open();
  p.advance();
  p.close(m, CstKind::PrefixOperator);
  Some(k)
}

/// Tries to get an **infix** operator, or `None` and no input was consumed.
fn try_infix_operator(p: &mut Parser) -> Option<OperatorKind> {
  debug_assert_ne!(p.peek(), Whitespace);
  debug_assert_ne!(p.peek(), Comment);
  let k = match p.peek() {
    ColonColon => OperatorKind::Path,
    Dot => OperatorKind::FieldAccess,
    Star => OperatorKind::Mul,
    Slash => OperatorKind::Div,
    Percent => OperatorKind::Rem,
    Plus => OperatorKind::Add,
    Minus => OperatorKind::Sub,
    AmpersandEqual => OperatorKind::BitAndAssign,
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
    LessThan => {
      let m = p.open();
      p.advance();
      return Some(match p.peek() {
        LessThan => {
          p.advance();
          return Some(match p.peek() {
            Equal => {
              p.advance();
              p.close(m, CstKind::InfixOperator);
              OperatorKind::ShiftLeftAssign
            }
            _ => {
              p.close(m, CstKind::InfixOperator);
              OperatorKind::ShiftLeft
            }
          });
        }
        Equal => {
          p.advance();
          p.close(m, CstKind::InfixOperator);
          OperatorKind::CmpLe
        }
        _ => {
          p.close(m, CstKind::InfixOperator);
          OperatorKind::CmpLt
        }
      });
    }
    GreaterThan => {
      let m = p.open();
      p.advance();
      return Some(match p.peek() {
        GreaterThan => {
          p.advance();
          return Some(match p.peek() {
            Equal => {
              p.advance();
              p.close(m, CstKind::InfixOperator);
              OperatorKind::ShiftRightAssign
            }
            _ => {
              p.close(m, CstKind::InfixOperator);
              OperatorKind::ShiftRight
            }
          });
        }
        Equal => {
          p.advance();
          p.close(m, CstKind::InfixOperator);
          OperatorKind::CmpGe
        }
        _ => {
          p.close(m, CstKind::InfixOperator);
          OperatorKind::CmpGt
        }
      });
    }
    Ampersand => {
      let m = p.open();
      p.advance();
      return Some(match p.peek() {
        Ampersand => {
          p.advance();
          p.close(m, CstKind::InfixOperator);
          OperatorKind::ConditionalAnd
        }
        _ => {
          p.close(m, CstKind::InfixOperator);
          OperatorKind::BitAnd
        }
      });
    }
    Pipe => {
      let m = p.open();
      p.advance();
      return Some(match p.peek() {
        Pipe => {
          p.advance();
          p.close(m, CstKind::InfixOperator);
          OperatorKind::ConditionalOr
        }
        _ => {
          p.close(m, CstKind::InfixOperator);
          OperatorKind::BitOr
        }
      });
    }
    _ => return None,
  };
  let m = p.open();
  p.advance();
  p.close(m, CstKind::InfixOperator);
  Some(k)
}

/// Tries to get a **postfix** operator, or `None` and no input was consumed.
fn try_postfix_operator(p: &mut Parser) -> Option<OperatorKind> {
  debug_assert_ne!(p.peek(), Whitespace);
  debug_assert_ne!(p.peek(), Comment);
  let k = match p.peek() {
    OpParen => OperatorKind::FnCall,
    OpBracket => OperatorKind::ArrayIndex,
    Question => OperatorKind::Try,
    KwAs => OperatorKind::As,
    _ => return None,
  };
  //debug_assert_eq!(k.binding().1, BindDirection::Ambiguious, "{k:?}");
  let m = p.open();
  p.advance();
  p.close(m, CstKind::PostfixOperator);
  Some(k)
}

/// Parse a value atom, or `None` for no input consumed.
fn try_val_atom(p: &mut Parser) -> Option<CloseMark> {
  debug_assert_ne!(p.peek(), Whitespace);
  debug_assert_ne!(p.peek(), Comment);
  Some(match p.peek() {
    KwTrue | KwFalse | Ident | LitNum | LitStr => {
      let m = p.open();
      p.advance();
      while let Whitespace = p.peek() {
        p.advance();
      }
      p.close(m, CstKind::AtomicValue)
    }
    OpParen => {
      let m = p.open();
      p.advance();
      try_val_expr(p, 0);
      p.advance(); // todo: expect ClParen
      while let Whitespace = p.peek() {
        p.advance();
      }
      p.close(m, CstKind::ParenGroup)
    }
    _ => return None,
  })
}

/// Parse a value atom, or `None` for no input consumed.
fn try_val_expr(p: &mut Parser, min_bp: u8) -> Option<CloseMark> {
  debug_assert_ne!(p.peek(), Whitespace);
  debug_assert_ne!(p.peek(), Comment);

  todo!("I give up, this doesn't make any sense");
}

mod tests {
  use super::*;
  #[test]
  fn test_try_prefix_operator() {
    let mut p = Parser::new(tokenize("-").collect());
    assert_eq!(try_prefix_operator(&mut p), Some(OperatorKind::Negative));
    p.build_tree();
    let mut p = Parser::new(tokenize("!").collect());
    assert_eq!(try_prefix_operator(&mut p), Some(OperatorKind::BitNot));
    p.build_tree();
    let mut p = Parser::new(tokenize("*").collect());
    assert_eq!(try_prefix_operator(&mut p), Some(OperatorKind::Dereference));
    p.build_tree();
    let mut p = Parser::new(tokenize("return").collect());
    assert_eq!(try_prefix_operator(&mut p), Some(OperatorKind::Return));
    p.build_tree();
    let mut p = Parser::new(tokenize("break").collect());
    assert_eq!(try_prefix_operator(&mut p), Some(OperatorKind::Break));
    p.build_tree();
  }
  #[test]
  fn test_try_infix_operator() {
    let mut p = Parser::new(tokenize("::").collect());
    assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::Path));
    p.build_tree();
    let mut p = Parser::new(tokenize(".").collect());
    assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::FieldAccess));
    p.build_tree();
    let mut p = Parser::new(tokenize("*").collect());
    assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::Mul));
    p.build_tree();
    let mut p = Parser::new(tokenize("/").collect());
    assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::Div));
    p.build_tree();
    let mut p = Parser::new(tokenize("%").collect());
    assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::Rem));
    p.build_tree();
    let mut p = Parser::new(tokenize("+").collect());
    assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::Add));
    p.build_tree();
    let mut p = Parser::new(tokenize("-").collect());
    assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::Sub));
    p.build_tree();
    let mut p = Parser::new(tokenize("<<").collect());
    assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::ShiftLeft));
    p.build_tree();
    let mut p = Parser::new(tokenize(">>").collect());
    assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::ShiftRight));
    p.build_tree();
    let mut p = Parser::new(tokenize("&").collect());
    assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::BitAnd));
    p.build_tree();
    let mut p = Parser::new(tokenize("^").collect());
    assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::BitXor));
    p.build_tree();
    let mut p = Parser::new(tokenize("|").collect());
    assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::BitOr));
    p.build_tree();
    let mut p = Parser::new(tokenize("==").collect());
    assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::CmpEq));
    p.build_tree();
    let mut p = Parser::new(tokenize("!=").collect());
    assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::CmpNe));
    p.build_tree();
    let mut p = Parser::new(tokenize("<").collect());
    assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::CmpLt));
    p.build_tree();
    let mut p = Parser::new(tokenize(">").collect());
    assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::CmpGt));
    p.build_tree();
    let mut p = Parser::new(tokenize("<=").collect());
    assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::CmpLe));
    p.build_tree();
    let mut p = Parser::new(tokenize(">=").collect());
    assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::CmpGe));
    p.build_tree();
    let mut p = Parser::new(tokenize("&&").collect());
    assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::ConditionalAnd));
    p.build_tree();
    let mut p = Parser::new(tokenize("||").collect());
    assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::ConditionalOr));
    p.build_tree();
    let mut p = Parser::new(tokenize("..").collect());
    assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::RangeExclusive));
    p.build_tree();
    let mut p = Parser::new(tokenize("..=").collect());
    assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::RangeInclusive));
    p.build_tree();
    let mut p = Parser::new(tokenize("=").collect());
    assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::Assign));
    p.build_tree();
    let mut p = Parser::new(tokenize("+=").collect());
    assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::AddAssign));
    p.build_tree();
    let mut p = Parser::new(tokenize("-=").collect());
    assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::SubAssign));
    p.build_tree();
    let mut p = Parser::new(tokenize("*=").collect());
    assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::MulAssign));
    p.build_tree();
    let mut p = Parser::new(tokenize("/=").collect());
    assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::DivAssign));
    p.build_tree();
    let mut p = Parser::new(tokenize("%=").collect());
    assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::RemAssign));
    p.build_tree();
    let mut p = Parser::new(tokenize("&=").collect());
    assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::BitAndAssign));
    p.build_tree();
    let mut p = Parser::new(tokenize("|=").collect());
    assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::BitOrAssign));
    p.build_tree();
    let mut p = Parser::new(tokenize("^=").collect());
    assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::BitXorAssign));
    p.build_tree();
    let mut p = Parser::new(tokenize("<<=").collect());
    assert_eq!(try_infix_operator(&mut p), Some(OperatorKind::ShiftLeftAssign));
    p.build_tree();
    let mut p = Parser::new(tokenize(">>=").collect());
    assert_eq!(
      try_infix_operator(&mut p),
      Some(OperatorKind::ShiftRightAssign)
    );
    p.build_tree();
  }
  #[test]
  fn test_try_postfix_operator() {
    let mut p = Parser::new(tokenize("(").collect());
    assert_eq!(try_postfix_operator(&mut p), Some(OperatorKind::FnCall));
    p.build_tree();
    let mut p = Parser::new(tokenize("[").collect());
    assert_eq!(try_postfix_operator(&mut p), Some(OperatorKind::ArrayIndex));
    p.build_tree();
    let mut p = Parser::new(tokenize("as").collect());
    assert_eq!(try_postfix_operator(&mut p), Some(OperatorKind::As));
    p.build_tree();
  }
}
