#![allow(unused_imports)]
//! Concrete Syntax Tree module.

use str_id::StrId;

use crate::Span;
use crate::cst::actions::do_module;
use crate::cst::parser::CstParser;
use crate::operators::InfixOperator;
use crate::operators::PostfixOperator;
use crate::operators::PrefixOperator;
use crate::tokenizer::Token;
use crate::tokenizer::TokenKind;
use crate::tokenizer::TokenKind::Comment;
use crate::tokenizer::TokenKind::Whitespace;

pub mod actions;
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
impl core::fmt::Display for Cst {
  /// Better way to look at the tree than Debug provides.
  ///
  /// * use the alternate flag to enable displaying of whitespace and comment
  ///   elements. Otherwise they are skipped from the output.
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
          CstElem::SubTree(cst) => {
            fmt_rec(cst, f, indents + 2)?;
          }
          CstElem::FixedToken(token_kind, span) => {
            for _ in 0..(indents + 2) {
              write!(f, " ")?;
            }
            if let Some(s) = token_kind.fixed_str() {
              write!(f, "{s}")?;
            } else {
              write!(f, "{token_kind:?}")?;
            }
            if let Some(span) = span {
              writeln!(f, " @({span:?})")?;
            } else {
              writeln!(f)?;
            }
          }
          CstElem::Whitespace(_, span) => {
            if f.alternate() {
              for _ in 0..(indents + 2) {
                write!(f, " ")?;
              }
              write!(f, "Whitespace")?;
              if let Some(span) = span {
                writeln!(f, " @({span:?})")?;
              } else {
                writeln!(f)?;
              }
            }
          }
          CstElem::Comment(_, span) => {
            if f.alternate() {
              for _ in 0..(indents + 2) {
                write!(f, " ")?;
              }
              write!(f, "Comment")?;
              if let Some(span) = span {
                writeln!(f, " @({span:?})")?;
              } else {
                writeln!(f)?;
              }
            }
          }
          CstElem::Identifier(string, span) => {
            for _ in 0..(indents + 2) {
              write!(f, " ")?;
            }
            write!(f, "Identifier({string:?})")?;
            if let Some(span) = span {
              writeln!(f, " @({span:?})")?;
            } else {
              writeln!(f)?;
            }
          }
          CstElem::LitNumber(string, span) => {
            for _ in 0..(indents + 2) {
              write!(f, " ")?;
            }
            write!(f, "LitNumber({string:?})")?;
            if let Some(span) = span {
              writeln!(f, " @({span:?})")?;
            } else {
              writeln!(f)?;
            }
          }
          CstElem::LitString(string, span) => {
            for _ in 0..(indents + 2) {
              write!(f, " ")?;
            }
            write!(f, "LitString({string:?})")?;
            if let Some(span) = span {
              writeln!(f, " @({span:?})")?;
            } else {
              writeln!(f)?;
            }
          }
          CstElem::ErrorBytes(_, span) => {
            for _ in 0..(indents + 2) {
              write!(f, " ")?;
            }
            write!(f, "ErrorBytes")?;
            if let Some(span) = span {
              writeln!(f, " @({span:?})")?;
            } else {
              writeln!(f)?;
            }
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum CstKind {
  #[default]
  ErrCstKind,

  /// * The `Module` tag should only contain `Item` trees.
  Module,
  /// * The first non-trivial element of each `Item` should be an item keyword
  Item,

  ParensGroup,
  BracketGroup,
  BraceGroup,

  Statement,
  ExprVal,
  ExprType,
  OperatorInfix(InfixOperator),
  OperatorPrefix(PrefixOperator),
  OperatorPostfix(PostfixOperator),
}

/// A single element within a [Cst].
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CstElem {
  /// An entire inner tree
  SubTree(Cst),
  /// A fixed-text token (keyword or punctuation)
  FixedToken(TokenKind, Option<Span>),
  /// Whitespace text.
  Whitespace(String, Option<Span>),
  /// Comment text
  Comment(String, Option<Span>),
  /// An identifier.
  Identifier(String, Option<Span>),
  /// A literal number.
  LitNumber(String, Option<Span>),
  /// A literal string.
  LitString(String, Option<Span>),
  /// Raw error bytes.
  ErrorBytes(Vec<u8>, Option<Span>),
}
