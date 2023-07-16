use crate::{ast::Ast, *};

#[derive(Clone, PartialEq, Eq)]
pub enum Label {
  Ident(StaticStr),
  Number(Result<i32, CowStr>),
}
impl Label {
  pub fn parser<'a, I>() -> impl Parser<'a, I, Self, ErrRichTokenTree<'a>>
  where
    I: ValueInput<'a, Token = TokenTree, Span = SimpleSpan>,
  {
    let ident = ident().map(Self::Ident);

    let num =
      num_lit().validate(|lit, _span, _emitter| lit_to_value(lit)).map(Self::Number);

    choice((ident, num)).then_ignore(colon())
  }
}
impl core::fmt::Debug for Label {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Self::Number(Ok(n)) => write!(f, "{n}:"),
      Self::Number(Err(e)) => write!(f, "{e:?}:"),
      Self::Ident(i) => write!(f, "{i}:"),
    }
  }
}

#[test]
fn test_parser() {
  let checks: &[(&str, Label)] = &[
    //
    ("here:", Label::Ident("here")),
    ("1:", Label::Number(Ok(1))),
    ("$FF00:", Label::Number(Ok(0xFF00))),
    ("%11:", Label::Number(Ok(0b11))),
  ];

  for (prog, expected) in checks {
    let tokens = Ast::tokenize(prog.to_string());
    let token_trees =
      crate::token_tree::make_token_trees(&tokens.items).into_output().unwrap();
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
