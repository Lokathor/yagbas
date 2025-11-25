use super::*;
use chumsky::prelude::*;
use chumsky::recovery::Strategy;
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

#[inline]
#[allow(unused)]
fn span32(start: u32, end: u32) -> Span32 {
  Span32 { start, end, context: () }
}

use crate::Token::*;
use crate::TokenTree::*;

/// Lets you `select_ref!` the content out of some `Braces`
pub fn braces_content_p<'src>() -> impl YagParser<'src, YagParserInput<'src>> {
  select_ref! {
    TokenTree::Braces(b) = ex => make_yag_parser_input(b),
  }
}
/// Lets you `select_ref!` the content out of some `Brackets`
pub fn brackets_content_p<'src>() -> impl YagParser<'src, YagParserInput<'src>>
{
  select_ref! {
    TokenTree::Brackets(b) = ex => make_yag_parser_input(b),
  }
}
/// Lets you `select_ref!` the content out of some `Parens`
pub fn parens_content_p<'src>() -> impl YagParser<'src, YagParserInput<'src>> {
  select_ref! {
    TokenTree::Parens(b) = ex => make_yag_parser_input(b),
  }
}

pub fn kw_bitbag_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwBitbag) => ()
  }
}
pub fn kw_break_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwBreak) => ()
  }
}
pub fn kw_const_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwConst) => ()
  }
}
pub fn kw_continue_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwContinue) => ()
  }
}
pub fn kw_else_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwElse) => ()
  }
}
pub fn kw_fn_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwFn) => ()
  }
}
pub fn kw_if_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwIf) => ()
  }
}
pub fn kw_let_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwLet) => ()
  }
}
pub fn kw_loop_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwLoop) => ()
  }
}
pub fn kw_mmio_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwMmio) => ()
  }
}
pub fn kw_mut_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwMut) => ()
  }
}
pub fn kw_return_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwReturn) => ()
  }
}
pub fn kw_rom_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwRom) => ()
  }
}
pub fn kw_static_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwStatic) => ()
  }
}
pub fn kw_struct_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwStruct) => ()
  }
}

pub fn punct_comma_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(Comma) => ()
  }
}
pub fn punct_equal_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(Equal) => ()
  }
}
pub fn punct_hash_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(Hash) => ()
  }
}

pub fn ident_p<'src>() -> impl YagParser<'src, StrID> {
  select! {
    Lone(Ident) = ex => {
      let state: &SimpleState<YagParserState> = ex.state();
      let source: &str = state.source;
      let span: Span32 = ex.span();
      let range: Range<usize> = span.start.try_into().unwrap()..span.end.try_into().unwrap();
      let str_id = StrID::from(&source[range]);
      str_id
    }
  }
}
pub fn num_lit_p<'src>() -> impl YagParser<'src, StrID> {
  select! {
    Lone(NumLit) = ex => {
      let state: &SimpleState<YagParserState> = ex.state();
      let source: &str = state.source;
      let span: Span32 = ex.span();
      let range: Range<usize> = span.start.try_into().unwrap()..span.end.try_into().unwrap();
      let str_id = StrID::from(&source[range]);
      str_id
    }
  }
}
pub fn bool_p<'src>() -> impl YagParser<'src, bool> {
  select! {
    Lone(KwTrue) => true,
    Lone(KwFalse) => false,
  }
}

/// looks like `# [ EXPR ]`
pub fn attribute_p<'src>() -> impl YagParser<'src, Expr> {
  punct_hash_p().ignore_then(expr_p().nested_in(brackets_content_p()))
  // TODO: if we read a hash and stuff in brackets, then even if the stuff isn't
  // a valid expression we should record it as an attribute we found (with an
  // error kind)
}

pub fn expr_p<'src>() -> impl YagParser<'src, Expr> {
  chumsky::prelude::todo()
}

pub fn statement_p<'src>() -> impl YagParser<'src, Statement> {
  chumsky::prelude::todo()
}
