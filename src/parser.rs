use crate::*;

use chumsky::{input::MappedInput, prelude::*, recursive::Indirect};

pub fn items_of<'src>(
  trees: &'src [(TokenTree, Span32)], yag_state: YagParserState,
) -> (Vec<AstItem>, Vec<Rich<'src, TokenTree, Span32>>) {
  let _ = trees;
  let _ = yag_state;
  panic!()
}

#[derive(Debug, Clone, Copy)]
pub struct YagParserState {
  pub source: &'static str,
  pub file_id: FileID,
}

pub type YagParserInput<'src> = MappedInput<
  'src,
  TokenTree,
  Span32,
  &'src [(TokenTree, Span32)],
  fn(&'src (TokenTree, Span32)) -> (&'src TokenTree, &'src Span32),
>;

pub type YagParserExtra<'src> =
  Full<Rich<'src, TokenTree, Span32>, SimpleState<YagParserState>, ()>;

pub type YagRecursive<'b, 'src, T> =
  Recursive<Indirect<'src, 'b, YagParserInput<'src>, T, YagParserExtra<'src>>>;

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
  Parser<'src, YagParserInput<'src>, O, YagParserExtra<'src>> + Clone
{
}

impl<'src, O, T> YagParser<'src, O> for T
where
  T: Parser<'src, YagParserInput<'src>, O, YagParserExtra<'src>>,
  T: Clone,
{
}

/// Lets us assert a particular expected output type for the parser.
///
/// This is necesary because rust doesn't allow a let binding to use "impl
/// trait" as the type declaration.
///
/// No runtime effect.
pub fn assert_output_ty<'src, T>(_: &impl YagParser<'src, T>) {}

#[test]
fn test_impl_return_readabilty() {
  #[allow(dead_code)]
  fn example_parser<'src>() -> impl YagParser<'src, ()> {
    chumsky::prelude::todo()
  }
}
