use crate::{
  ast::token::Token::{self, *},
  src_files::{FileSpan, FileSpanned},
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
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Self::Lone(t) => core::fmt::Debug::fmt(&t, f),
      Self::Parens(tts) => fmt_tt_list("(", ")", tts, f),
      Self::Brackets(tts) => fmt_tt_list("[", "]", tts, f),
      Self::Braces(tts) => fmt_tt_list("{", "}", tts, f),
      Self::TreeError => write!(f, "TreeError"),
    }
  }
}

fn fmt_tt_list(
  open: &str, close: &str, tts: &[FileSpanned<TokenTree>],
  f: &mut core::fmt::Formatter<'_>,
) -> core::fmt::Result {
  let skip_threshold = 100;
  write!(f, "{open}")?;
  if tts.len() > skip_threshold {
    write!(f, "...{} elements...", tts.len())?;
  } else {
    for (i, tt) in tts.iter().enumerate() {
      if i > 0 {
        write!(f, ", ")?;
      }
      core::fmt::Debug::fmt(&tt, f)?;
    }
  }
  write!(f, "{close}")?;
  Ok(())
}
