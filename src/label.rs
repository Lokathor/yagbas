use crate::*;

#[derive(Clone, PartialEq, Eq)]
pub enum Label {
  Ident(StaticStr),
  Number(StaticStr),
}
impl Label {
  pub fn parser<'a, I>() -> impl Parser<'a, I, Self, ErrRichTokenTree<'a>>
  where
    I: ValueInput<'a, Token = TokenTree, Span = SimpleSpan>,
  {
    choice((
      //
      ident().map(Self::Ident),
      num_lit().map(Self::Number),
    ))
    .then_ignore(colon())
  }
}
impl core::fmt::Debug for Label {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Self::Number(n) => write!(f, "{n}:"),
      Self::Ident(i) => write!(f, "{i}:"),
    }
  }
}

#[test]
fn test_label_parser() {
  let checks: &[(&str, Label)] = &[
    //
    ("here:", Label::Ident("here")),
    ("1:", Label::Number("1")),
    ("$FF00:", Label::Number("$FF00")),
    ("%11:", Label::Number("%11")),
  ];

  for (prog, expected) in checks {
    let tokens = crate::comment_filter::no_comment_tokens(prog).unwrap();
    let token_trees = crate::token_tree::make_token_trees(&tokens).into_output().unwrap();
    let parse_result = run_parser(Label::parser(), &token_trees);
    if parse_result.has_errors() {
      for err in parse_result.errors() {
        println!("ERROR: {err:?}");
      }
      panic!("One or more errors.");
    }
    assert_eq!(expected, parse_result.output().unwrap());
  }
}
