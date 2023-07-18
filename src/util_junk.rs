use crate::static_str::StaticStr;

use super::*;

use chumsky::{input::SpannedInput, prelude::*, *};

pub type ErrRichToken<'a> = extra::Err<Rich<'a, Token>>;
pub type ErrRichTokenTree<'a> = extra::Err<Rich<'a, TokenTree>>;
pub type TokenSlice<'a> = SpannedInput<Token, SimpleSpan, &'a [(Token, SimpleSpan)]>;
pub type InputSlice<'a, T> = SpannedInput<T, SimpleSpan, &'a [(T, SimpleSpan)]>;
pub type TokenTreeSlice<'a> =
  SpannedInput<TokenTree, SimpleSpan, &'a [(TokenTree, SimpleSpan)]>;

/// "Identity, 2-arg"
///
/// This just wraps the two values as a tuple. This is only really useful as a
/// higher order function to pass to map and similar when we want to join
/// multi-arg inputs into a single value output.
#[inline]
#[must_use]
pub const fn id2<A, B>(a: A, b: B) -> (A, B) {
  (a, b)
}

// TODO: there's something not great about the naming here where two methods
// match anything and then one of them takes a specific input to match. it seems
// like the naming is inconsistent.

pub fn ident<'a>(
) -> impl Parser<'a, TokenTreeSlice<'a>, StaticStr, ErrRichTokenTree<'a>> + Clone {
  select! {
    Lone(Ident(i)) => i,
  }
}

pub fn num_lit<'a>(
) -> impl Parser<'a, TokenTreeSlice<'a>, StaticStr, ErrRichTokenTree<'a>> + Clone {
  select! {
    Lone(NumLit(n)) => n,
  }
}

pub fn punct<'a>(
  punct: char,
) -> impl Parser<'a, TokenTreeSlice<'a>, char, ErrRichTokenTree<'a>> + Clone {
  select! {
    Lone(Punct(p)) if p == punct => punct,
  }
}

pub fn plus<'a>(
) -> impl Parser<'a, TokenTreeSlice<'a>, char, ErrRichTokenTree<'a>> + Clone {
  punct('+')
}

pub fn minus<'a>(
) -> impl Parser<'a, TokenTreeSlice<'a>, char, ErrRichTokenTree<'a>> + Clone {
  punct('-')
}

pub fn bang<'a>(
) -> impl Parser<'a, TokenTreeSlice<'a>, char, ErrRichTokenTree<'a>> + Clone {
  punct('!')
}

pub fn semicolon<'a>(
) -> impl Parser<'a, TokenTreeSlice<'a>, char, ErrRichTokenTree<'a>> + Clone {
  punct(';')
}

pub fn comma<'a>(
) -> impl Parser<'a, TokenTreeSlice<'a>, char, ErrRichTokenTree<'a>> + Clone {
  punct(',')
}

pub fn pipe<'a>(
) -> impl Parser<'a, TokenTreeSlice<'a>, char, ErrRichTokenTree<'a>> + Clone {
  punct('|')
}

pub fn caret<'a>(
) -> impl Parser<'a, TokenTreeSlice<'a>, char, ErrRichTokenTree<'a>> + Clone {
  punct('^')
}

pub fn colon<'a>(
) -> impl Parser<'a, TokenTreeSlice<'a>, char, ErrRichTokenTree<'a>> + Clone {
  punct(':')
}

pub fn ampersand<'a>(
) -> impl Parser<'a, TokenTreeSlice<'a>, char, ErrRichTokenTree<'a>> + Clone {
  punct('&')
}
