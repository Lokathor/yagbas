use chumsky::{input::ValueInput, prelude::*};

use crate::{
  parser::{DebugListWithoutSpans, ErrRichToken},
  token::{Token, Token::*},
};

/// A lone token or a list of tokens in one of three grouping styles.
///
/// Collecting a raw token list into token trees ensures that all the
/// opening/closing markers of all the groupings are balanced before trying to
/// do any more advanced parsing.
///
/// * See: [make_token_trees]
#[derive(Clone, PartialEq, Eq)]
pub enum TokenTree {
  Lone(Token),
  Parens(Vec<(Self, SimpleSpan)>),
  Brackets(Vec<(Self, SimpleSpan)>),
  Braces(Vec<(Self, SimpleSpan)>),
}
use TokenTree::*;
impl core::fmt::Debug for TokenTree {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Lone(t) => core::fmt::Debug::fmt(&t, f),
      Parens(ts) => {
        if ts.len() > 10 {
          write!(f, "Parens({} len)", ts.len())
        } else {
          write!(f, "Parens({:?})", DebugListWithoutSpans(ts))
        }
      }
      Brackets(ts) => write!(f, "Brackets({:?})", DebugListWithoutSpans(ts)),
      Braces(ts) => write!(f, "Braces({:?})", DebugListWithoutSpans(ts)),
    }
  }
}
impl TokenTree {
  /// Parses for just one token tree.
  pub fn parser<'a, I>() -> impl Parser<'a, I, Self, ErrRichToken<'a>>
  where
    I: ValueInput<'a, Token = crate::token::Token, Span = SimpleSpan>,
  {
    recursive(|tt| {
      let token_list = tt.map_with_span(|i, s| (i, s)).repeated().collect::<Vec<_>>();

      // Looks like `(...)`
      let parens = token_list
        .clone()
        .delimited_by(just(Punct('(')), just(Punct(')')))
        .map(TokenTree::Parens);

      // Looks like `[...]`
      let brackets = token_list
        .clone()
        .delimited_by(just(Punct('[')), just(Punct(']')))
        .map(TokenTree::Brackets);

      // Looks like `{...}`
      let braces = token_list
        .clone()
        .delimited_by(just(Punct('{')), just(Punct('}')))
        .map(TokenTree::Braces);

      // Looks like something that does *NOT* close one of the other types.
      let single = none_of([Punct(')'), Punct(']'), Punct('}')]).map(TokenTree::Lone);

      parens.or(brackets).or(braces).or(single)
    })
  }
}
