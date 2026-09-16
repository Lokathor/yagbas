#![allow(unused_imports)]
//! Concrete Syntax Tree module.

use std::string::FromUtf8Error;

use str_id::StrId;

use crate::Span;
use crate::cst::actions::gather_module;
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
/// Unlike an Abstract Syntax Tree, the Concrete syntax tree preserves **all**
/// data about the source code, and can recreate a source file exactly. This is
/// an explicit layer in the compiler because one day hopefully Yagbas will have
/// a code re-formatter, which would operate on Cst data instead of only an Ast,
/// so that comments can be shifted around safely.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Cst {
  /// This is a hint about the intended form of the Cst's elements.
  pub kind: CstKind,
  /// The elements of this tree, possibly including entire inner trees.
  pub elements: Vec<CstElem>,
}
impl Cst {
  /// Convert a Cst back to source code.
  ///
  /// If the Cst hasn't been altered since it came out of the parser, then it
  /// will be an exact match with the source code parsed.
  ///
  /// ## Failure
  /// * If the Cst contains error bytes they can cause the output to not be
  ///   valid utf-8.
  pub fn to_source_code(&self) -> Result<String, FromUtf8Error> {
    let mut buf = Vec::new();
    recursive_helper(self, &mut buf);
    return String::try_from(buf);

    fn recursive_helper(cst: &Cst, buf: &mut Vec<u8>) {
      for element in &cst.elements {
        match element {
          CstElem::SubTree(cst) => recursive_helper(cst, buf),
          CstElem::FixedToken(token_kind, _span) => {
            let s = token_kind.fixed_str().unwrap_or("");
            buf.extend_from_slice(s.as_bytes());
          }
          CstElem::Whitespace(string, _span)
          | CstElem::Comment(string, _span)
          | CstElem::Identifier(string, _span)
          | CstElem::LitNumber(string, _span)
          | CstElem::LitString(string, _span) => {
            buf.extend_from_slice(string.as_bytes());
          }
          CstElem::ErrorBytes(items, _span) => {
            buf.extend_from_slice(items);
          }
        }
      }
    }
  }
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
      writeln!(f, "{:?}", s.kind)?;
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
      #[cfg(false)]
      for _ in 0..indents {
        write!(f, " ")?;
      }
      //writeln!(f, "}}")?;
      Ok(())
    }
  }
}

/// The kinds of Cst tree that the [CstParser] can generate.
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum CstKind {
  #[default]
  ErrCstKindDefault,

  // I think that it's better to have fewer kinds exist when possible. I'm not
  // totally sure why I think that.
  Module,
  Item,
  ParensGroup,
  BracketGroup,
  BraceGroup,
  Statement,
  ExprVal,
  ExprType,
  Pattern,
  OperatorInfix(InfixOperator),
  OperatorPrefix(PrefixOperator),
  OperatorPostfix(PostfixOperator),
}

/// A single element within a [Cst].
///
/// Span data is available and accurate when the `Cst` was created via normal
/// parsing of a source file. If the Cst has been edited since creation, or was
/// created in memory, the spans cannot be trusted and might not be present.
/// Because of this, the data for varying token types is extracted from the
/// source immediately during Cst creation, and tagged according to the token
/// kind that it came from.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CstElem {
  /// An entire inner tree.
  SubTree(Cst),
  /// A fixed-text token (keyword or punctuation)
  FixedToken(TokenKind, Option<Span>),
  /// Whitespace text.
  Whitespace(String, Option<Span>),
  /// Comment text.
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
impl CstElem {
  pub fn sub_tree(&self) -> Option<&Cst> {
    if let CstElem::SubTree(cst) = self { Some(cst) } else { None }
  }
  pub fn fixed_token(&self) -> Option<(TokenKind, Option<Span>)> {
    if let CstElem::FixedToken(k, s) = self { Some((*k, *s)) } else { None }
  }
  pub fn whitespace(&self) -> Option<(&str, Option<Span>)> {
    if let CstElem::Whitespace(st, s) = self {
      Some((st.as_str(), *s))
    } else {
      None
    }
  }
  pub fn comment(&self) -> Option<(&str, Option<Span>)> {
    if let CstElem::Comment(st, s) = self {
      Some((st.as_str(), *s))
    } else {
      None
    }
  }
  pub fn identifier(&self) -> Option<(&str, Option<Span>)> {
    if let CstElem::Identifier(st, s) = self {
      Some((st.as_str(), *s))
    } else {
      None
    }
  }
  pub fn lit_number(&self) -> Option<(&str, Option<Span>)> {
    if let CstElem::LitNumber(st, s) = self {
      Some((st.as_str(), *s))
    } else {
      None
    }
  }
  pub fn lit_string(&self) -> Option<(&str, Option<Span>)> {
    if let CstElem::LitString(st, s) = self {
      Some((st.as_str(), *s))
    } else {
      None
    }
  }
  pub fn error_bytes(&self) -> Option<(&[u8], Option<Span>)> {
    if let CstElem::ErrorBytes(st, s) = self {
      Some((st.as_slice(), *s))
    } else {
      None
    }
  }
}
