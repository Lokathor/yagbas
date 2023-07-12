use crate::{instruction::Instruction, *};

#[derive(Clone, PartialEq, Eq)]
pub struct ConstDecl {
  pub name: (StaticStr, SimpleSpan),
  pub expr: Vec<(TokenTree, SimpleSpan)>,
}
impl ConstDecl {
  pub fn parser<'a, I>() -> impl Parser<'a, I, Self, ErrRichTokenTree<'a>>
  where
    I: ValueInput<'a, Token = TokenTree, Span = SimpleSpan>,
  {
    let kw_const = just(Lone(KwConst));

    let args =
      none_of(Lone(Punct(';'))).map_with_span(id2).repeated().collect::<Vec<_>>();

    kw_const
      .ignore_then(ident().map_with_span(id2))
      .then_ignore(equal())
      .then(args)
      .then_ignore(semicolon())
      .map(|(name, args)| Self { name, expr: args })
  }
}
impl core::fmt::Debug for ConstDecl {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "const {:?} = ", self.name.0)?;
    for (i, (x, _)) in self.expr.iter().enumerate() {
      if i > 0 {
        write!(f, " ")?;
      }
      write!(f, "{x:?}")?;
    }
    write!(f, ";")?;
    Ok(())
  }
}

#[test]
fn test_parser() {
  let checks: &[(&str, ConstDecl)] = &[
    //
    (
      "const FOO = 10;",
      ConstDecl {
        name: ("FOO", SimpleSpan::from(6..9)),
        expr: vec![(Lone(NumLit("10")), SimpleSpan::from(12..14))],
      },
    ),
    (
      "const JOYP = $FF00;",
      ConstDecl {
        name: ("JOYP", SimpleSpan::from(6..10)),
        expr: vec![(Lone(NumLit("$FF00")), SimpleSpan::from(13..18))],
      },
    ),
  ];

  for (prog, expected) in checks {
    let tokens = crate::comment_filter::no_comment_tokens(prog).unwrap();
    let token_trees = crate::token_tree::make_token_trees(&tokens).into_output().unwrap();
    let parse_result = run_parser(ConstDecl::parser(), &token_trees);
    if parse_result.has_errors() {
      for err in parse_result.errors() {
        println!("ERROR: {err:?}");
      }
      panic!("One or more errors.");
    }
    assert_eq!(expected, parse_result.output().unwrap());
  }
}
