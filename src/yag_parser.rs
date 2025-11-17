use crate::{Span32, TokenTree, YagError};
use chumsky::{
  Parser,
  error::Rich,
  extra::{Full, SimpleState},
  input::{BorrowInput, Input, ValueInput},
};

#[derive(Debug, Clone, Copy)]
pub struct YagParserState {}

#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
pub struct YagParserInput<'src>(&'src [(TokenTree, Span32)]);

impl<'src> Input<'src> for YagParserInput<'src> {
  type Span = Span32;
  type Token = TokenTree;
  type MaybeToken = &'src TokenTree;
  type Cursor = usize;
  type Cache = Self;

  fn begin(self) -> (usize, Self) {
    (0, self)
  }
  fn cursor_location(cursor: &usize) -> usize {
    *cursor
  }
  unsafe fn next_maybe(
    cache: &mut Self, cursor: &mut usize,
  ) -> Option<&'src TokenTree> {
    cache.0.get(*cursor).map(|tree_span| {
      *cursor += 1;
      &tree_span.0
    })
  }
  unsafe fn span(cache: &mut Self, range: core::ops::Range<&usize>) -> Span32 {
    let s: Span32 = cache.0[*range.start].1;
    let e: Span32 = cache.0[*range.end].1;
    Span32 { start: s.start, end: e.end, context: () }
  }
}

impl<'src> ValueInput<'src> for YagParserInput<'src> {
  unsafe fn next(cache: &mut Self, cursor: &mut usize) -> Option<TokenTree> {
    unsafe { Self::next_maybe(cache, cursor) }.cloned()
  }
}

impl<'src> BorrowInput<'src> for YagParserInput<'src> {
  unsafe fn next_ref(
    cache: &mut Self, cursor: &mut usize,
  ) -> Option<&'src TokenTree> {
    unsafe { Self::next_maybe(cache, cursor) }
  }
}

pub trait YagParser<'src, O>:
  Parser<
    'src,
    YagParserInput<'src>,
    O,
    Full<Rich<'src, TokenTree, Span32>, SimpleState<YagParserState>, ()>,
  > + Clone
{
}

impl<'src, O, T> YagParser<'src, O> for T
where
  T: Parser<
      'src,
      YagParserInput<'src>,
      O,
      Full<Rich<'src, TokenTree, Span32>, SimpleState<YagParserState>, ()>,
    >,
  T: Clone,
{
}

#[test]
fn test_impl_return_readabilty() {
  #[allow(unused)]
  fn example_parser<'src, I>() -> impl YagParser<'src, ()> {
    chumsky::prelude::todo()
  }
}
