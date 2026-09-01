#![allow(unused_imports)]
//! Concrete Syntax Tree module.

use std::ops::Range;

use crate::cst::actions::do_module;
use crate::cst::operators::InfixOperator;
use crate::cst::operators::PostfixOperator;
use crate::cst::operators::PrefixOperator;
use crate::cst::parser::CstParser;
use crate::tokenizer::Token;
use crate::tokenizer::TokenKind;
use crate::tokenizer::TokenKind::Comment;
use crate::tokenizer::TokenKind::Whitespace;

pub mod actions;
pub mod operators;
pub mod parser;

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
  /// Generates the Cst for a module of source code.
  ///
  /// This never fails, but the resulting Cst can contain any number of
  /// error locations.
  pub fn from_module_src(src: &str) -> Self {
    let mut p = CstParser::new(src);
    do_module(&mut p);
    p.build_tree()
  }
  /// Strip all `Whitespace` and `Comment` tokens from the tree, recursively.
  pub fn strip_trivia(&mut self) {
    let mut i = 0;
    while i < self.elements.len() {
      match &mut self.elements[i] {
        CstElem::Token(Token { kind: Whitespace | Comment, .. }) => {
          self.elements.remove(i);
          continue;
        }
        CstElem::Tree(cst) => {
          cst.strip_trivia();
        }
        _ => (),
      }
      i += 1;
    }
  }
  /// If this Cst has an error.
  ///
  /// An error can be this Cst itself, or it could be any Token or SubTree
  /// within this Cst.
  pub fn has_error(&self) -> bool {
    self.kind.is_error()
      || self.elements.iter().any(|el| match el {
        CstElem::Token(token) => token.is_error(),
        CstElem::Tree(cst) => cst.has_error(),
      })
  }
  /// Walks the tree and asserts that no token or sub-tree is an error.
  #[track_caller]
  pub fn assert_no_errors(&self) {
    assert!(!self.kind.is_error(), "Bad Kind: {:?}", self.kind);
    for elemnt in &self.elements {
      match elemnt {
        CstElem::Token(token) => {
          assert!(!token.is_error(), "Bad Token: {token:?}")
        }
        CstElem::Tree(cst) => cst.assert_no_errors(),
      }
    }
  }
  /// Iterator over **only** the elements of this Cst which are a sub-tree.
  pub fn sub_trees(&self) -> impl Iterator<Item = &Cst> + '_ {
    self.elements.iter().filter_map(|element| match element {
      CstElem::Token(_token) => None,
      CstElem::Tree(cst) => Some(cst),
    })
  }
  /// Iterator over **only** the tokens directly at this level.
  pub fn tokens_here(&self) -> impl Iterator<Item = Token> + '_ {
    self.elements.iter().filter_map(|element| match element {
      CstElem::Token(token) => Some(*token),
      CstElem::Tree(_cst) => None,
    })
  }
  /// Iter over elements but skip `Whitespace` and `Comment` token elements.
  pub fn iter_important(&self) -> impl Iterator<Item = &CstElem> {
    self.elements.iter().filter(|el| {
      !matches!(el, CstElem::Token(Token { kind: Whitespace | Comment, .. }))
    })
  }
  /// Gets the span of this tree within the source.
  pub fn span_within(&self, src: &str) -> Range<usize> {
    let mut out = 0..0;
    if let Some(el) = self.elements.first() {
      out.start = match el {
        CstElem::Token(token) => token.position as usize,
        CstElem::Tree(cst) => cst.span_within(src).start,
      };
    }
    if let Some(el) = self.elements.last() {
      out.end = match el {
        CstElem::Token(token) => token.span_within(src).end,
        CstElem::Tree(cst) => cst.span_within(src).end,
      };
    }
    out
  }
}
impl core::fmt::Display for Cst {
  /// Better way to look at the tree than Debug provides.
  ///
  /// * use the alternate flag to enable displaying of whitespace and comment tokens, as well as commentary syntax trees. Otherwise they are skipped from the output.
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    return fmt_rec(self, f, 0);

    fn fmt_rec(
      s: &Cst, f: &mut core::fmt::Formatter<'_>, indents: usize,
    ) -> core::fmt::Result {
      use core::fmt::Write;
      for _ in 0..indents {
        write!(f, " ")?;
      }
      writeln!(f, "{:?} {{", s.kind)?;
      for element in &s.elements {
        match element {
          CstElem::Token(Token { kind, position }) => {
            if !f.alternate()
              && (*kind == TokenKind::Comment || *kind == TokenKind::Whitespace)
            {
              continue;
            }
            for _ in 0..(indents + 2) {
              write!(f, " ")?;
            }
            writeln!(f, "{kind:?} @({position:?})")?;
          }
          CstElem::Tree(cst) => {
            fmt_rec(cst, f, indents + 2)?;
          }
        }
      }
      for _ in 0..indents {
        write!(f, " ")?;
      }
      writeln!(f, "}}")
    }
  }
}

/// I have no idea what the correct set of tags is here!
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CstKind {
  ErrNoTreeKindSet,
  ErrGeneric,
  ErrExpectedItemKeyword,
  ErrExpectedValueExpression,
  ErrExpectedTypeExpression,
  ErrNeedsParensToDisambiguate,
  ErrTodo,
  ErrExpected(TokenKind),
  ErrExpectedBody,
  ErrExpectedIfCondition,
  ErrUnbalancedAngleMarks,
  //
  Module,
  ItemFunction,
  ItemStaticMmio,
  ItemStaticRam,
  ItemStaticRom,
  ItemConst,
  //
  ArgumentList,
  FnCallArgument,
  ReturnType,
  Body,
  //
  StmtLet,
  StmtItem,
  StmtExpression,
  StmtEmpty,
  //
  ExprVal,
  ExprType,
  ExprForVar,
  ExprForRange,
  //
  OperatorInfix(InfixOperator),
  OperatorPrefix(PrefixOperator),
  OperatorPostfix(PostfixOperator),
  //
  MmioLocation,
}
impl CstKind {
  /// If this tree kind is some sort of error.
  pub const fn is_error(self) -> bool {
    use CstKind::*;
    matches!(
      self,
      ErrExpected(_)
        | ErrExpectedBody
        | ErrExpectedIfCondition
        | ErrExpectedItemKeyword
        | ErrExpectedTypeExpression
        | ErrExpectedValueExpression
        | ErrGeneric
        | ErrNeedsParensToDisambiguate
        | ErrNoTreeKindSet
        | ErrTodo
        | ErrUnbalancedAngleMarks
    )
  }
  /// If this tree kind is some sort of error.
  pub const fn is_statement(self) -> bool {
    use CstKind::*;
    matches!(self, StmtEmpty | StmtExpression | StmtItem | StmtLet)
  }
}

/// A single element within a [Cst].
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CstElem {
  Token(Token),
  Tree(Cst),
}
impl CstElem {
  /// When this is a `Token` variant, gives the wrapped [Token].
  pub const fn token(&self) -> Option<Token> {
    match self {
      CstElem::Token(token) => Some(*token),
      CstElem::Tree(_) => None,
    }
  }
  /// When this is a `Tree` variant, gives the wrapped [Cst] (by ref).
  pub const fn tree(&self) -> Option<&Cst> {
    match self {
      CstElem::Token(_) => None,
      CstElem::Tree(cst) => Some(cst),
    }
  }
}
