use super::*;

use chumsky::{extra::ParserExtra, input::SpannedInput, prelude::*, *};

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

pub fn ident<'a>(
) -> impl Parser<'a, TokenTreeSlice<'a>, StrID, ErrRichTokenTree<'a>> + Clone + Copy {
  select! {
    Lone(Ident(i)) => i,
  }
}

pub fn num_lit<'a>(
) -> impl Parser<'a, TokenTreeSlice<'a>, StrID, ErrRichTokenTree<'a>> + Clone + Copy {
  select! {
    Lone(NumLit(n)) => n,
  }
}

pub fn braces<'a>(
) -> impl Parser<'a, TokenTreeSlice<'a>, Vec<(TokenTree, SimpleSpan)>, ErrRichTokenTree<'a>>
     + Clone
     + Copy {
  select! {
    Braces(b) => b,
  }
}

pub fn punct<'a>(
) -> impl Parser<'a, TokenTreeSlice<'a>, char, ErrRichTokenTree<'a>> + Clone + Copy {
  select! {
    Lone(Punct(p)) => p,
  }
}

pub fn plus<'a>(
) -> impl Parser<'a, TokenTreeSlice<'a>, char, ErrRichTokenTree<'a>> + Clone + Copy {
  select! {
    Lone(Punct('+')) => '+',
  }
}

pub fn minus<'a>(
) -> impl Parser<'a, TokenTreeSlice<'a>, char, ErrRichTokenTree<'a>> + Clone + Copy {
  select! {
    Lone(Punct('-')) => '-',
  }
}

pub fn bang<'a>(
) -> impl Parser<'a, TokenTreeSlice<'a>, char, ErrRichTokenTree<'a>> + Clone + Copy {
  select! {
    Lone(Punct('!')) => '!',
  }
}

pub fn semicolon<'a>(
) -> impl Parser<'a, TokenTreeSlice<'a>, char, ErrRichTokenTree<'a>> + Clone + Copy {
  select! {
    Lone(Punct(';')) => ';',
  }
}

pub fn comma<'a>(
) -> impl Parser<'a, TokenTreeSlice<'a>, char, ErrRichTokenTree<'a>> + Clone + Copy {
  select! {
    Lone(Punct(',')) => ',',
  }
}

pub fn pipe<'a>(
) -> impl Parser<'a, TokenTreeSlice<'a>, char, ErrRichTokenTree<'a>> + Clone + Copy {
  select! {
    Lone(Punct('|')) => '|',
  }
}

pub fn caret<'a>(
) -> impl Parser<'a, TokenTreeSlice<'a>, char, ErrRichTokenTree<'a>> + Clone + Copy {
  select! {
    Lone(Punct('^')) => '^',
  }
}

pub fn colon<'a>(
) -> impl Parser<'a, TokenTreeSlice<'a>, char, ErrRichTokenTree<'a>> + Clone + Copy {
  select! {
    Lone(Punct(':')) => ':',
  }
}

pub fn ampersand<'a>(
) -> impl Parser<'a, TokenTreeSlice<'a>, char, ErrRichTokenTree<'a>> + Clone + Copy {
  select! {
    Lone(Punct('&')) => '&',
  }
}

pub fn equal<'a>(
) -> impl Parser<'a, TokenTreeSlice<'a>, char, ErrRichTokenTree<'a>> + Clone + Copy {
  select! {
    Lone(Punct('=')) => '=',
  }
}

pub fn eat_until_parser<'a, P, O>(
  p: P,
) -> impl Parser<'a, TokenTreeSlice<'a>, (), ErrRichTokenTree<'a>> + Clone
where
  P: Parser<'a, InputSlice<'a, TokenTree>, O, ErrRichTokenTree<'a>> + Clone,
{
  let with_mark = any().and_is(p.clone().not()).ignored().repeated().then_ignore(p);
  let without_mark = any().ignored().repeated().at_least(1);
  choice((with_mark, without_mark)).ignored()
}

pub fn eat_until_semicolon_or_braces<'a>(
) -> impl Parser<'a, TokenTreeSlice<'a>, (), ErrRichTokenTree<'a>> + Clone {
  eat_until_parser(choice((semicolon().ignored(), braces().ignored())))
}
