use chumsky::span::SimpleSpan;
use yagbas_srcfiletypes::{FileData, Token, TokenTree, tokens_of, trees_of};

#[test]
fn check_tokens_of() {
  let expected: Vec<(Token, SimpleSpan)> = vec![
    (Token::KwA, (0..1).into()),
    (Token::Plus, (2..3).into()),
    (Token::KwB, (4..5).into()),
  ];
  let actual: Vec<(Token, SimpleSpan)> = tokens_of("a + b");
  assert_eq!(expected, actual);
}

#[test]
fn check_trees_of() {
  let expected = vec![
    (TokenTree::Lone(Token::KwLoop), (0..4).into()),
    (
      TokenTree::Braces(vec![(TokenTree::Lone(Token::KwA), (7..8).into())]),
      (5..10).into(),
    ),
  ];
  let (actual, errs) = trees_of("loop { a }");
  assert_eq!(expected, actual);
  assert!(errs.is_empty());
}

#[test]
fn check_in_memory() {
  let file_data = FileData::in_memory("a b c".into());
  let expected = "a";
  let actual = file_data.get_span((0..1).into()).unwrap();
  assert_eq!(expected, actual);
}
