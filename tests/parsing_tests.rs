use chumsky::{extra::ParserExtra, input::Input, span::SimpleSpan, ParseResult, Parser};
use yagbas::{
  const_expr::ConstExpr,
  token::{tokenize_module, Token},
  token_tree::{grow_token_trees, TokenTree},
  util_junk::*,
};
use Token::*;
use TokenTree::*;

#[allow(unused)]
macro_rules! span {
  ($s:expr) => {
    SimpleSpan::from($s)
  };
}

#[inline]
pub fn run_tt_parser<'a, P, O, E>(
  parser: P, data: &'a [(TokenTree, SimpleSpan)],
) -> ParseResult<O, E::Error>
where
  P: Parser<'a, InputSlice<'a, TokenTree>, O, E>,
  E: ParserExtra<'a, InputSlice<'a, TokenTree>>,
  <E as ParserExtra<'a, InputSlice<'a, TokenTree>>>::State: Default,
  <E as ParserExtra<'a, InputSlice<'a, TokenTree>>>::Context: Default,
{
  // calculate the likely span value based on the first and last token, assumes
  // that the tokens are still properly in order.
  let span: SimpleSpan = if data.is_empty() {
    (0..0).into()
  } else {
    let end = data.last().unwrap().1.end;
    (end..end).into()
  };
  let input = data.spanned(span);
  parser.parse(input)
}

#[test]
fn test_const_expr_parser() {
  let checks = [
    //
    ("0", ConstExpr::Value(0)),
    ("$FA", ConstExpr::Value(0xFA)),
    ("0xFA", ConstExpr::Value(0xFA)),
    ("0b101", ConstExpr::Value(0b101)),
    ("%101", ConstExpr::Value(0b101)),
    ("(12)", ConstExpr::Value(12)),
    ("((12))", ConstExpr::Value(12)),
    ("FOO", ConstExpr::Ident("FOO")),
    (
      "FOO + 1",
      ConstExpr::Add(
        Box::new((ConstExpr::Ident("FOO"), SimpleSpan::from(0..3))),
        Box::new((ConstExpr::Value(1), SimpleSpan::from(6..7))),
      ),
    ),
    (
      "FOO-3",
      ConstExpr::Sub(
        Box::new((ConstExpr::Ident("FOO"), SimpleSpan::from(0..3))),
        Box::new((ConstExpr::Value(3), SimpleSpan::from(4..5))),
      ),
    ),
    ("-7", ConstExpr::Value(-7)),
    ("+8", ConstExpr::Value(8)),
    ("- 7", ConstExpr::Value(-7)),
    ("+ 8", ConstExpr::Value(8)),
    ("1 + 1", ConstExpr::Value(2)),
    ("1+1", ConstExpr::Value(2)),
    ("(1) + 1", ConstExpr::Value(2)),
    ("1 + (1)", ConstExpr::Value(2)),
    ("(1) + (1)", ConstExpr::Value(2)),
    ("!0", ConstExpr::Value(!0_i32)),
    ("1 | 2", ConstExpr::Value(1 | 2)),
    ("567 ^ 8910", ConstExpr::Value(567_i32 ^ 8910)),
    ("292 & 1995", ConstExpr::Value(292_i32 & 1995)),
    ("u8::MAX", ConstExpr::Value(u8::MAX as i32)),
    ("u16::MAX", ConstExpr::Value(u16::MAX as i32)),
    (
      "size_of_static!(tiles)",
      ConstExpr::MacroUse {
        name: ("size_of_static", SimpleSpan::from(0..14)),
        args: vec![(Lone(Ident("tiles")), SimpleSpan::from(16..21))],
      },
    ),
  ];
  for (src, expected) in checks.into_iter() {
    let tokens: Vec<(Token, SimpleSpan)> = tokenize_module(src);
    let (token_trees, tree_parse_errors) = grow_token_trees(&tokens);
    assert!(tree_parse_errors.is_empty(), "{tree_parse_errors:?}");

    let (opt_const_expr, expr_parse_errors) =
      run_tt_parser(ConstExpr::parser(), &token_trees).into_output_errors();
    assert!(expr_parse_errors.is_empty(), "{expr_parse_errors:?}");
    assert_eq!(expected, opt_const_expr.unwrap());
  }
  //
}
