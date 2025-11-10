use crate::{Span32, TokenTree, YagError};
use chumsky::{
  Parser,
  error::Rich,
  extra::{Full, SimpleState},
  input::{BorrowInput, ValueInput},
};

#[derive(Debug, Clone, Copy)]
pub struct YagParserState {}

pub trait YagParserInput<'src>:
  BorrowInput<'src, Token = TokenTree, Span = Span32> + ValueInput<'src>
{
}

pub trait YagParser<'src, I, O>:
  Parser<
    'src,
    I,
    O,
    Full<Rich<'src, TokenTree, Span32>, SimpleState<YagParserState>, ()>,
  > + Clone
where
  I: YagParserInput<'src>,
{
}

impl<'src, I, O, T> YagParser<'src, I, O> for T
where
  T: Parser<
      'src,
      I,
      O,
      Full<Rich<'src, TokenTree, Span32>, SimpleState<YagParserState>, ()>,
    >,
  T: Clone,
  I: YagParserInput<'src>,
{
}

#[test]
fn test_impl_return_readabilty() {
  #[allow(unused)]
  fn example_parser<'src, I>() -> impl YagParser<'src, I, ()>
  where
    I: YagParserInput<'src>,
  {
    chumsky::prelude::todo()
  }
}
