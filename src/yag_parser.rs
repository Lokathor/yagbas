use crate::{Span32, TokenTree, YagError};
use chumsky::{
  Parser,
  error::Rich,
  extra::{Full, SimpleState},
  input::{BorrowInput, Input, MappedInput, ValueInput},
};

#[derive(Debug, Clone, Copy)]
pub struct YagParserState {
  pub source: &'static str,
}

pub type YagParserInput<'src> = MappedInput<
  TokenTree,
  Span32,
  &'src [(TokenTree, Span32)],
  fn(&'src (TokenTree, Span32)) -> (&'src TokenTree, &'src Span32),
>;

fn mapper<'src>(
  (tt, span): &'src (TokenTree, Span32),
) -> (&'src TokenTree, &'src Span32) {
  (tt, span)
}

pub fn make_yag_parser_input<'src>(
  s: &'src [(TokenTree, Span32)],
) -> YagParserInput<'src> {
  let eoi: Span32 = s.last().map(|(_tt, span)| *span).unwrap_or_default();
  Input::map(s, eoi, mapper)
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
  #[allow(dead_code)]
  fn example_parser<'src>() -> impl YagParser<'src, ()> {
    chumsky::prelude::todo()
  }
}
