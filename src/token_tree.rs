use crate::{
  src_files::{FileSpan, FileSpanned},
  token::Token::{self, *},
};
use chumsky::{
  input::{BorrowInput, Input, ValueInput},
  prelude::*,
};

/// A lone token or a list of token trees within one of three groupings.
///
/// Collecting a raw token list into token trees ensures that all the
/// opening/closing markers of all the groupings are balanced before trying to
/// do any more advanced parsing.
#[derive(Clone, PartialEq, Eq)]
pub enum TokenTree {
  Lone(Token),
  Parens(Vec<FileSpanned<TokenTree>>),
  Brackets(Vec<FileSpanned<TokenTree>>),
  Braces(Vec<FileSpanned<TokenTree>>),
  TreeError,
}

impl core::fmt::Debug for TokenTree {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let skip_threshold = 100;
    match self {
      Self::Lone(t) => core::fmt::Debug::fmt(&t, f),
      Self::Parens(ts) => {
        if ts.len() > skip_threshold {
          write!(f, "(...{} elements...)", ts.len())
        } else {
          write!(f, "(")?;
          for (i, tt) in ts.iter().enumerate() {
            if i > 0 {
              write!(f, " ")?;
            }
            write!(f, "{tt:?}")?;
          }
          write!(f, ")")?;
          Ok(())
        }
      }
      Self::Brackets(ts) => {
        if ts.len() > skip_threshold {
          write!(f, "[...{} elements...]", ts.len())
        } else {
          write!(f, "[")?;
          for (i, tt) in ts.iter().enumerate() {
            if i > 0 {
              write!(f, " ")?;
            }
            write!(f, "{tt:?}")?;
          }
          write!(f, "]")?;
          Ok(())
        }
      }
      Self::Braces(ts) => {
        if ts.len() > skip_threshold {
          write!(f, "{{...{} elements...}}", ts.len())
        } else {
          write!(f, "{{")?;
          for (i, tt) in ts.iter().enumerate() {
            if i > 0 {
              write!(f, " ")?;
            }
            write!(f, "{tt:?}")?;
          }
          write!(f, "}}")?;
          Ok(())
        }
      }
      Self::TreeError => write!(f, "TreeError"),
    }
  }
}
