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
