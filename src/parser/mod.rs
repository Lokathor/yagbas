use core::ops::{Deref, DerefMut, Range};

use crate::{lexer::Token, StaticStr};
use chumsky::{
  extra::ParserExtra,
  input::{BoxedStream, SpannedInput, Stream, ValueInput},
  prelude::*,
  primitive::*,
  select, Parser,
};
use logos::Span;

pub mod comment_filter;
pub mod const_decl;
pub mod item;
pub mod section;
pub mod token_tree;

use comment_filter::*;
use const_decl::*;
use item::*;
use section::*;
use token_tree::*;
use Token::*;
use TokenTree::*;

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

pub struct DebugListWithoutSpans<'a, T>(pub &'a [(T, SimpleSpan)]);
impl<'a, T> core::fmt::Debug for DebugListWithoutSpans<'a, T>
where
  T: core::fmt::Debug,
{
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let mut x = f.debug_list();
    for (t, _) in self.0 {
      x.entry(t);
    }
    x.finish()
  }
}

pub type ErrRichToken<'a> = extra::Err<Rich<'a, Token>>;
pub type ErrRichTokenTree<'a> = extra::Err<Rich<'a, TokenTree>>;
pub type InputSlice<'a, T> = SpannedInput<T, SimpleSpan, &'a [(T, SimpleSpan)]>;

/// Runs a parser from `T` to `O` on a slice of `T`, giving a [ParseResult]
#[inline]
pub fn run_parser<'a, P, T, O, E>(
  parser: P, data: &'a [(T, SimpleSpan)],
) -> ParseResult<O, E::Error>
where
  P: Parser<'a, InputSlice<'a, T>, O, E>,
  E: ParserExtra<'a, InputSlice<'a, T>>,
  <E as ParserExtra<'a, InputSlice<'a, T>>>::State: Default,
  <E as ParserExtra<'a, InputSlice<'a, T>>>::Context: Default,
{
  // calculate the likely span value based on the first and last token, assumes
  // that the tokens are still properly in order.
  let span: SimpleSpan = if data.is_empty() {
    (0..0).into()
  } else {
    let start = data.first().unwrap().1.start;
    let end = data.last().unwrap().1.end;
    (start..end).into()
  };
  let input = data.spanned(span);
  parser.parse(input)
}
