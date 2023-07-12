use crate::{instruction::Instruction, *};

#[derive(Clone, PartialEq, Eq)]
pub struct SectionDecl {
  pub name: (StaticStr, SimpleSpan),
  pub location: Vec<(TokenTree, SimpleSpan)>,
  pub block: Vec<(TokenTree, SimpleSpan)>,
}
impl SectionDecl {
  pub fn parser<'a, I>() -> impl Parser<'a, I, Self, ErrRichTokenTree<'a>>
  where
    I: ValueInput<'a, Token = TokenTree, Span = SimpleSpan>,
  {
    let kw_section = just(Lone(KwSection));

    kw_section
      .ignore_then(ident().map_with_span(id2))
      .then(bracket_group())
      .then(brace_group())
      .map(|((name, location), block)| Self { name, location, block })
  }
}
impl core::fmt::Debug for SectionDecl {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "section {:?} [ ", self.name.0)?;
    for (i, (x, _)) in self.location.iter().enumerate() {
      if i > 0 {
        write!(f, " ")?;
      }
      write!(f, "{x:?}")?;
    }
    write!(f, " ] {{ ")?;
    for (i, (x, _)) in self.block.iter().enumerate() {
      if i > 0 {
        write!(f, " ")?;
      }
      write!(f, "{x:?}")?;
    }
    write!(f, " }}")?;
    Ok(())
  }
}

#[test]
fn test_parser() {
  let checks: &[(&str, SectionDecl)] = &[
    //
    (
      "section main [rom0] { nop; }",
      SectionDecl {
        name: ("main", SimpleSpan::from(8..12)),
        location: vec![(Lone(Ident("rom0")), SimpleSpan::from(14..18))],
        block: vec![
          (Lone(InstNOP), SimpleSpan::from(22..25)),
          (Lone(Punct(';')), SimpleSpan::from(25..26)),
        ],
      },
    ),
  ];

  for (prog, expected) in checks {
    let tokens = crate::comment_filter::no_comment_tokens(prog).unwrap();
    let token_trees = crate::token_tree::make_token_trees(&tokens).into_output().unwrap();
    let parse_result = run_parser(SectionDecl::parser(), &token_trees);
    if parse_result.has_errors() {
      for err in parse_result.errors() {
        println!("ERROR: {err:?}");
      }
      panic!("One or more errors.");
    }
    assert_eq!(expected, parse_result.output().unwrap());
  }
}
