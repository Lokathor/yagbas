#![allow(unused)]
#![warn(missing_docs)]

//! Module for all the parsing junk!

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
}

/// Concrete Syntax Tree
///
/// Use the `pretty_debug` method if you need to print debug info nicely.
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Cst {
  pub kind: CstKind,
  pub elements: Vec<CstElem>,
}
impl Cst {
  /// pretty-print the debug info of this Cst into a String.
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

/// I have no idea what the correct set of tags is here!
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CstKind {
  ErrNoTreeKindSet,
  ErrGeneric,
  ErrExpectedValueExpression,
  //
  ValExpr,
  AtomicValue,
  ParenGroup,
  InfixOperator,
  PrefixOperator,
  PostfixOperator,
  //
  Argument,
}

/// A single element within a [Cst].
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CstElem {
  #[allow(missing_docs)]
  Token(Token),
  #[allow(missing_docs)]
  Tree(Cst),
}

/// Operator binding direction.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BindDirection {
  /// always binds left
  Left,
  /// always binds right
  Right,
  /// requires parentheses
  Ambiguious,
}

/// All the kinds of operator in Yagbas.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OperatorKind {
  /// `x::y`
  Path,
  /// `x.y`
  Access,
  /// `x()`
  FnCall,
  /// `x[y]`
  ArrayIndex,
  /// `x?`
  Try,
  /// `-x`
  Negative,
  /// `!x`
  BitNot,
  /// `*x`
  Dereference,
  /// `&x`
  Reference,
  /// `x as y`
  As,
  /// `x*y`
  Mul,
  /// `x/y`
  Div,
  /// `x%y`
  Rem,
  /// `x+y`
  Add,
  /// `x-y`
  Sub,
  /// `x<<y`
  ShiftLeft,
  /// `x>>y`
  ShiftRight,
  /// `x&y`
  BitAnd,
  /// `x^y`
  BitXor,
  /// `x|y`
  BitOr,
  /// `x==y`
  CmpEq,
  /// `x!=y`
  CmpNe,
  /// `x<y`
  CmpLt,
  /// `x>y`
  CmpGt,
  /// `x<=y`
  CmpLe,
  /// `x>=y`
  CmpGe,
  /// `x&&y`
  ConditionalAnd,
  /// `x||y`
  ConditionalOr,
  /// `x..y`, `x..`, `..y`, and `..`
  RangeExclusive,
  /// `x..=y`, `x..=`, `..=y`, and `..=`
  RangeInclusive,
  /// `x=y`
  Assign,
  /// `x+=y`
  AddAssign,
  /// `x-=y`
  SubAssign,
  /// `x*=y`
  MulAssign,
  /// `x/=y`
  DivAssign,
  /// `x%=y`
  RemAssign,
  /// `x&=y`
  BitAndAssign,
  /// `x|=y`
  BitOrAssign,
  /// `x^=y`
  BitXorAssign,
  /// `x>>=y`
  ShiftLeftAssign,
  /// `x<<=y`
  ShiftRightAssign,
  /// `return x`
  Return,
  /// `break x`
  Break,
}
impl OperatorKind {
  /// Gives the bind strength and direction for this operator.
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
      OperatorKind::Access => (34, BindDirection::Left),
      OperatorKind::Path => (36, BindDirection::Left),
    }
  }
}

/// Tries to get a **prefix** operator, or `None` and no input was consumed.
fn try_prefix_operator(p: &mut Parser) -> Option<(OperatorKind, CloseMark)> {
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
  Some((k, p.close(m, CstKind::PrefixOperator)))
}

/// Tries to get an **infix** operator, or `None` and no input was consumed.
fn try_infix_operator(p: &mut Parser) -> Option<(OperatorKind, CloseMark)> {
  debug_assert_ne!(p.peek(), Whitespace);
  debug_assert_ne!(p.peek(), Comment);
  let k = match p.peek() {
    ColonColon => OperatorKind::Path,
    Dot => OperatorKind::Access,
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
              (
                OperatorKind::ShiftLeftAssign,
                p.close(m, CstKind::InfixOperator),
              )
            }
            _ => (OperatorKind::ShiftLeft, p.close(m, CstKind::InfixOperator)),
          });
        }
        Equal => {
          p.advance();
          (OperatorKind::CmpLe, p.close(m, CstKind::InfixOperator))
        }
        _ => (OperatorKind::CmpLt, p.close(m, CstKind::InfixOperator)),
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
              (
                OperatorKind::ShiftRightAssign,
                p.close(m, CstKind::InfixOperator),
              )
            }
            _ => (OperatorKind::ShiftRight, p.close(m, CstKind::InfixOperator)),
          });
        }
        Equal => {
          p.advance();
          (OperatorKind::CmpGe, p.close(m, CstKind::InfixOperator))
        }
        _ => (OperatorKind::CmpGt, p.close(m, CstKind::InfixOperator)),
      });
    }
    Ampersand => {
      let m = p.open();
      p.advance();
      return Some(match p.peek() {
        Ampersand => {
          p.advance();
          (OperatorKind::ConditionalAnd, p.close(m, CstKind::InfixOperator))
        }
        _ => (OperatorKind::BitAnd, p.close(m, CstKind::InfixOperator)),
      });
    }
    Pipe => {
      let m = p.open();
      p.advance();
      return Some(match p.peek() {
        Pipe => {
          p.advance();
          (OperatorKind::ConditionalOr, p.close(m, CstKind::InfixOperator))
        }
        _ => (OperatorKind::BitOr, p.close(m, CstKind::InfixOperator)),
      });
    }
    _ => return None,
  };
  let m = p.open();
  p.advance();
  Some((k, p.close(m, CstKind::InfixOperator)))
}

/// Tries to get a **postfix** operator, or `None` and no input was consumed.
fn try_postfix_operator(p: &mut Parser) -> Option<(OperatorKind, CloseMark)> {
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
  Some((k, p.close(m, CstKind::PostfixOperator)))
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

/// Parse a value expression, or `None` for no input consumed.
fn try_val_expr(p: &mut Parser, min_bp: u8) -> Option<CloseMark> {
  // prefix or atom
  let mut lhs: CloseMark = if let Some((o, mark)) = try_prefix_operator(p) {
    let st = o.binding().0;
    let m = p.open_before(mark);
    if try_val_expr(p, st).is_none() {
      // allow return/break to have no operator
      if !matches!(o, OperatorKind::Return | OperatorKind::Break) {
        let opened = p.open();
        p.close(opened, CstKind::ErrExpectedValueExpression);
      }
    }
    p.close(m, CstKind::ValExpr)
  } else {
    try_val_atom(p)?
  };
  // infix/postfix
  let mut prevstr: Option<u8> = None;
  loop {
    let Some((o, postfix)) = peek_operator_post(p) else { break };
    let (strn, dir) = o.binding();
    let (lhs_bp, rhs_bp) = match dir {
      BindDirection::Left => (strn, strn + 1),
      BindDirection::Right => (strn + 1, strn),
      BindDirection::Ambiguious => (strn, strn + 1),
    };
    // TODO: this is the important part, this is why we need to *peek* the
    // operator before doing anything else, because we sometimes dont eat it
    // here, and instead eat it in the caller's stack frame.
    if lhs_bp < min_bp {
      // caller's operator, don't consume
      break;
    }
    if dir == BindDirection::Ambiguious && prevstr == Some(strn) {
      // the op should have parens (you should parse anyway for resilience)
    }
    prevstr = Some(strn);
    // now consume the operator
    // TODO: remove this, the op getter consumea it already.
    let m = p.open_before(lhs);
    let om = p.open();
    p.advance();
    let k =
      if postfix { CstKind::PostfixOperator } else { CstKind::InfixOperator };
    p.close(om, k);
    // if it was a postfix op, consume the non-expr tail, otherwise it was infix and we consume the rhs
    match o {
      OperatorKind::Try => {}
      OperatorKind::As => {
        // TODO: parse type here. needs its own parsing functions.
      }
      OperatorKind::ArrayIndex => {
        try_val_expr(p, 0);
        p.expect(ClBracket);
      }
      OperatorKind::RangeExclusive | OperatorKind::RangeInclusive => {
        // rhs is optional
        try_val_expr(p, rhs_bp);
      }
      OperatorKind::FnCall => {
        fn try_arg(p: &mut Parser) -> Option<CloseMark> {
          let ex = try_val_expr(p, 0)?;
          let m = p.open_before(ex);
          if !p.at(ClParen) {
            p.expect(Comma);
          }
          // you need to add this CstKind
          Some(p.close(m, CstKind::Argument))
        }
        while !p.at(ClParen) && p.has_more() {
          if try_arg(p).is_none() {
            p.advance_with_error("expected function argument");
          }
        }
        p.expect(ClParen);
      }
      _ => {
        // rhs
        if try_val_expr(p, rhs_bp).is_none() {
          let e = p.open();
          p.close(e, CstKind::ErrExpectedValueExpression);
        }
      }
    }
    lhs = p.close(m, CstKind::ValExpr);
  }
  Some(lhs)
}

// Peek at the next tokens. If they are an operator, which one,
// and is it postfix.
fn peek_operator_post(p: &mut Parser) -> Option<(OperatorKind, bool)> {
  if let Some(x) = try_prefix_operator(p) {
    Some((x.0, false))
  } else if let Some(x) = try_postfix_operator(p) {
    Some((x.0, true))
  } else {
    None
  }
}

mod tests {
  use super::*;
  #[test]
  fn test_try_prefix_operator() {
    let mut p = Parser::new(tokenize("-").collect());
    assert_eq!(try_prefix_operator(&mut p).unwrap().0, OperatorKind::Negative);
    p.build_tree();
    let mut p = Parser::new(tokenize("!").collect());
    assert_eq!(try_prefix_operator(&mut p).unwrap().0, OperatorKind::BitNot);
    p.build_tree();
    let mut p = Parser::new(tokenize("*").collect());
    assert_eq!(
      try_prefix_operator(&mut p).unwrap().0,
      OperatorKind::Dereference
    );
    p.build_tree();
    let mut p = Parser::new(tokenize("return").collect());
    assert_eq!(try_prefix_operator(&mut p).unwrap().0, OperatorKind::Return);
    p.build_tree();
    let mut p = Parser::new(tokenize("break").collect());
    assert_eq!(try_prefix_operator(&mut p).unwrap().0, OperatorKind::Break);
    p.build_tree();
  }
  #[test]
  fn test_try_infix_operator() {
    let mut p = Parser::new(tokenize("::").collect());
    assert_eq!(try_infix_operator(&mut p).unwrap().0, OperatorKind::Path);
    p.build_tree();
    let mut p = Parser::new(tokenize(".").collect());
    assert_eq!(try_infix_operator(&mut p).unwrap().0, OperatorKind::Access);
    p.build_tree();
    let mut p = Parser::new(tokenize("*").collect());
    assert_eq!(try_infix_operator(&mut p).unwrap().0, OperatorKind::Mul);
    p.build_tree();
    let mut p = Parser::new(tokenize("/").collect());
    assert_eq!(try_infix_operator(&mut p).unwrap().0, OperatorKind::Div);
    p.build_tree();
    let mut p = Parser::new(tokenize("%").collect());
    assert_eq!(try_infix_operator(&mut p).unwrap().0, OperatorKind::Rem);
    p.build_tree();
    let mut p = Parser::new(tokenize("+").collect());
    assert_eq!(try_infix_operator(&mut p).unwrap().0, OperatorKind::Add);
    p.build_tree();
    let mut p = Parser::new(tokenize("-").collect());
    assert_eq!(try_infix_operator(&mut p).unwrap().0, OperatorKind::Sub);
    p.build_tree();
    let mut p = Parser::new(tokenize("<<").collect());
    assert_eq!(try_infix_operator(&mut p).unwrap().0, OperatorKind::ShiftLeft);
    p.build_tree();
    let mut p = Parser::new(tokenize(">>").collect());
    assert_eq!(try_infix_operator(&mut p).unwrap().0, OperatorKind::ShiftRight);
    p.build_tree();
    let mut p = Parser::new(tokenize("&").collect());
    assert_eq!(try_infix_operator(&mut p).unwrap().0, OperatorKind::BitAnd);
    p.build_tree();
    let mut p = Parser::new(tokenize("^").collect());
    assert_eq!(try_infix_operator(&mut p).unwrap().0, OperatorKind::BitXor);
    p.build_tree();
    let mut p = Parser::new(tokenize("|").collect());
    assert_eq!(try_infix_operator(&mut p).unwrap().0, OperatorKind::BitOr);
    p.build_tree();
    let mut p = Parser::new(tokenize("==").collect());
    assert_eq!(try_infix_operator(&mut p).unwrap().0, OperatorKind::CmpEq);
    p.build_tree();
    let mut p = Parser::new(tokenize("!=").collect());
    assert_eq!(try_infix_operator(&mut p).unwrap().0, OperatorKind::CmpNe);
    p.build_tree();
    let mut p = Parser::new(tokenize("<").collect());
    assert_eq!(try_infix_operator(&mut p).unwrap().0, OperatorKind::CmpLt);
    p.build_tree();
    let mut p = Parser::new(tokenize(">").collect());
    assert_eq!(try_infix_operator(&mut p).unwrap().0, OperatorKind::CmpGt);
    p.build_tree();
    let mut p = Parser::new(tokenize("<=").collect());
    assert_eq!(try_infix_operator(&mut p).unwrap().0, OperatorKind::CmpLe);
    p.build_tree();
    let mut p = Parser::new(tokenize(">=").collect());
    assert_eq!(try_infix_operator(&mut p).unwrap().0, OperatorKind::CmpGe);
    p.build_tree();
    let mut p = Parser::new(tokenize("&&").collect());
    assert_eq!(
      try_infix_operator(&mut p).unwrap().0,
      OperatorKind::ConditionalAnd
    );
    p.build_tree();
    let mut p = Parser::new(tokenize("||").collect());
    assert_eq!(
      try_infix_operator(&mut p).unwrap().0,
      OperatorKind::ConditionalOr
    );
    p.build_tree();
    let mut p = Parser::new(tokenize("..").collect());
    assert_eq!(
      try_infix_operator(&mut p).unwrap().0,
      OperatorKind::RangeExclusive
    );
    p.build_tree();
    let mut p = Parser::new(tokenize("..=").collect());
    assert_eq!(
      try_infix_operator(&mut p).unwrap().0,
      OperatorKind::RangeInclusive
    );
    p.build_tree();
    let mut p = Parser::new(tokenize("=").collect());
    assert_eq!(try_infix_operator(&mut p).unwrap().0, OperatorKind::Assign);
    p.build_tree();
    let mut p = Parser::new(tokenize("+=").collect());
    assert_eq!(try_infix_operator(&mut p).unwrap().0, OperatorKind::AddAssign);
    p.build_tree();
    let mut p = Parser::new(tokenize("-=").collect());
    assert_eq!(try_infix_operator(&mut p).unwrap().0, OperatorKind::SubAssign);
    p.build_tree();
    let mut p = Parser::new(tokenize("*=").collect());
    assert_eq!(try_infix_operator(&mut p).unwrap().0, OperatorKind::MulAssign);
    p.build_tree();
    let mut p = Parser::new(tokenize("/=").collect());
    assert_eq!(try_infix_operator(&mut p).unwrap().0, OperatorKind::DivAssign);
    p.build_tree();
    let mut p = Parser::new(tokenize("%=").collect());
    assert_eq!(try_infix_operator(&mut p).unwrap().0, OperatorKind::RemAssign);
    p.build_tree();
    let mut p = Parser::new(tokenize("&=").collect());
    assert_eq!(
      try_infix_operator(&mut p).unwrap().0,
      OperatorKind::BitAndAssign
    );
    p.build_tree();
    let mut p = Parser::new(tokenize("|=").collect());
    assert_eq!(
      try_infix_operator(&mut p).unwrap().0,
      OperatorKind::BitOrAssign
    );
    p.build_tree();
    let mut p = Parser::new(tokenize("^=").collect());
    assert_eq!(
      try_infix_operator(&mut p).unwrap().0,
      OperatorKind::BitXorAssign
    );
    p.build_tree();
    let mut p = Parser::new(tokenize("<<=").collect());
    assert_eq!(
      try_infix_operator(&mut p).unwrap().0,
      OperatorKind::ShiftLeftAssign
    );
    p.build_tree();
    let mut p = Parser::new(tokenize(">>=").collect());
    assert_eq!(
      try_infix_operator(&mut p).unwrap().0,
      OperatorKind::ShiftRightAssign
    );
    p.build_tree();
  }
  #[test]
  fn test_try_postfix_operator() {
    let mut p = Parser::new(tokenize("(").collect());
    assert_eq!(try_postfix_operator(&mut p).unwrap().0, OperatorKind::FnCall);
    p.build_tree();
    let mut p = Parser::new(tokenize("[").collect());
    assert_eq!(
      try_postfix_operator(&mut p).unwrap().0,
      OperatorKind::ArrayIndex
    );
    p.build_tree();
    let mut p = Parser::new(tokenize("as").collect());
    assert_eq!(try_postfix_operator(&mut p).unwrap().0, OperatorKind::As);
    p.build_tree();
  }
}
