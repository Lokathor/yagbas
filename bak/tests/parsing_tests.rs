#![cfg(FALSE)]
#![allow(clippy::unit_cmp)]

use chumsky::{extra::ParserExtra, prelude::*};
use yagbas::{
  const_expr::ConstExpr,
  item::{ConstDecl, Item},
  str_id::StrID,
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
fn test_eat_until_semicolon_or_end() {
  let checks = [
    //
    (";", 1),
    ("foo;", 1),
    ("foo", 1),
    ("foo bar", 1),
    ("foo; bar;", 2),
    ("foo; {} bar;", 3),
  ];
  for (src, expected) in checks.into_iter() {
    let tokens: Vec<(Token, SimpleSpan)> = tokenize_module(src);
    let (token_trees, tree_parse_errors) = grow_token_trees(&tokens);
    assert!(tree_parse_errors.is_empty(), "{tree_parse_errors:?}");

    let (opt_list, expr_parse_errors) = run_tt_parser(
      eat_until_semicolon_or_braces().repeated().collect::<Vec<_>>(),
      &token_trees,
    )
    .into_output_errors();
    assert!(expr_parse_errors.is_empty(), "{expr_parse_errors:?}\nSrc: {src}\n");
    assert_eq!(expected, opt_list.unwrap().len(), "\nSrc Was: {src}\n");
  }
  //
}

#[test]
#[allow(clippy::precedence)]
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
    ("FOO", ConstExpr::Ident(StrID::from("FOO"))),
    ("-7", ConstExpr::Value(-7)),
    ("--7", ConstExpr::Value(7)),
    ("+8", ConstExpr::Value(8)),
    ("- 7", ConstExpr::Value(-7)),
    ("+ 8", ConstExpr::Value(8)),
    ("+ + 8", ConstExpr::Value(8)),
    (
      "FOO + 1",
      ConstExpr::Add(
        Box::new((ConstExpr::Ident(StrID::from("FOO")), SimpleSpan::from(0..3))),
        Box::new((ConstExpr::Value(1), SimpleSpan::from(6..7))),
      ),
    ),
    (
      "FOO-3",
      ConstExpr::Sub(
        Box::new((ConstExpr::Ident(StrID::from("FOO")), SimpleSpan::from(0..3))),
        Box::new((ConstExpr::Value(3), SimpleSpan::from(4..5))),
      ),
    ),
    ("1 + 1", ConstExpr::Value(2)),
    ("1+1", ConstExpr::Value(2)),
    ("(1) + 1", ConstExpr::Value(2)),
    ("1 + (1)", ConstExpr::Value(2)),
    ("(1) + (1)", ConstExpr::Value(2)),
    ("!0", ConstExpr::Value(!0_i32)),
    ("!!0", ConstExpr::Value(0_i32)),
    ("1 | 2", ConstExpr::Value(1 | 2)),
    ("567 ^ 8910", ConstExpr::Value(567_i32 ^ 8910)),
    ("292 & 1995", ConstExpr::Value(292_i32 & 1995)),
    ("u8::MAX", ConstExpr::Value(u8::MAX as i32)),
    ("u16::MAX", ConstExpr::Value(u16::MAX as i32)),
    ("i8::MAX", ConstExpr::Value(i8::MAX as i32)),
    ("i16::MAX", ConstExpr::Value(i16::MAX as i32)),
    ("i8::MIN", ConstExpr::Value(i8::MIN as i32)),
    ("i16::MIN", ConstExpr::Value(i16::MIN as i32)),
    (
      "size_of_static!(tiles)",
      ConstExpr::MacroUse {
        name: (StrID::from("size_of_static"), SimpleSpan::from(0..14)),
        args: vec![(Lone(Ident(StrID::from("tiles"))), SimpleSpan::from(16..21))],
      },
    ),
    ("1 + 1 + 1", ConstExpr::Value(3)),
    ("1 + 1 -8 ", ConstExpr::Value(-6)),
    ("2 | 3 & 7 ", ConstExpr::Value(2 | 3 & 7)),
    ("7 + 4 - 1 & 8  | 13", ConstExpr::Value(7 + 4 - 1 & 8 | 13)),
    ("7 + (4 - 1 ^ 123)  | 13", ConstExpr::Value(7 + (4 - 1 ^ 123) | 13)),
  ];
  for (src, expected) in checks.into_iter() {
    let tokens: Vec<(Token, SimpleSpan)> = tokenize_module(src);
    let (token_trees, tree_parse_errors) = grow_token_trees(&tokens);
    assert!(tree_parse_errors.is_empty(), "{tree_parse_errors:?}");

    let (opt_const_expr, expr_parse_errors) =
      run_tt_parser(ConstExpr::parser(), &token_trees).into_output_errors();
    assert!(expr_parse_errors.is_empty(), "{expr_parse_errors:?}\nSrc: {src}\n");
    assert_eq!(expected, opt_const_expr.unwrap(), "\nSrc Was: {src}\n");
  }
  //
}

#[test]
#[allow(clippy::precedence)]
fn test_const_decl_parser() {
  let checks = [
    //
    (
      "const FOO = $FF00;",
      ConstDecl {
        name: (StrID::from("FOO"), SimpleSpan::new(6, 9)),
        expr: (ConstExpr::Value(0xFF00), SimpleSpan::new(12, 17)),
      },
    ),
    (
      "const BAR = $FF00 + $AA;",
      ConstDecl {
        name: (StrID::from("BAR"), SimpleSpan::new(6, 9)),
        expr: (ConstExpr::Value(0xFFAA), SimpleSpan::new(12, 23)),
      },
    ),
    (
      "const;",
      ConstDecl {
        name: (StrID::from(""), SimpleSpan::new(5, 6)),
        expr: (ConstExpr::Err, SimpleSpan::new(5, 6)),
      },
    ),
    (
      "const",
      ConstDecl {
        name: (StrID::from(""), SimpleSpan::new(5, 5)),
        expr: (ConstExpr::Err, SimpleSpan::new(5, 5)),
      },
    ),
    (
      "const foo bar baz;",
      ConstDecl {
        name: (StrID::from(""), SimpleSpan::new(6, 18)),
        expr: (ConstExpr::Err, SimpleSpan::new(6, 18)),
      },
    ),
  ];
  for (src, expected) in checks.into_iter() {
    let tokens: Vec<(Token, SimpleSpan)> = tokenize_module(src);
    let (token_trees, tree_parse_errors) = grow_token_trees(&tokens);
    assert!(tree_parse_errors.is_empty(), "{tree_parse_errors:?}");

    let (opt_const_expr, _errors) =
      run_tt_parser(ConstDecl::parser(), &token_trees).into_output_errors();
    assert_eq!(Some(expected), opt_const_expr, "\nSrc Was: {src}\n");
  }
  //
}

#[test]
#[allow(clippy::precedence)]
fn test_item_parser() {
  let checks = [
    //
    (";", Item::ItemError),
    ("hello;", Item::ItemError),
    (
      "const FOO = $FF00;",
      Item::ConstDecl(ConstDecl {
        name: (StrID::from("FOO"), SimpleSpan::new(6, 9)),
        expr: (ConstExpr::Value(0xFF00), SimpleSpan::new(12, 17)),
      }),
    ),
    (
      "const FOO = ++++;",
      Item::ConstDecl(ConstDecl {
        name: (StrID::from("FOO"), SimpleSpan::new(6, 9)),
        expr: (ConstExpr::Err, SimpleSpan::new(12, 16)),
      }),
    ),
  ];
  for (src, expected) in checks.into_iter() {
    let tokens: Vec<(Token, SimpleSpan)> = tokenize_module(src);
    let (token_trees, tree_parse_errors) = grow_token_trees(&tokens);
    assert!(tree_parse_errors.is_empty(), "{tree_parse_errors:?}");

    let (opt_const_expr, _errors) =
      run_tt_parser(Item::parser(), &token_trees).into_output_errors();
    assert_eq!(Some(expected), opt_const_expr, "\nSrc Was: {src}\n");
  }
  //
}
