use crate::{instruction::Instruction, *};

#[derive(Clone, PartialEq, Eq)]
pub struct InstrUse {
  pub name: (Instruction, SimpleSpan),
  pub args: Vec<(TokenTree, SimpleSpan)>,
}
impl InstrUse {
  pub fn parser<'a, I>() -> impl Parser<'a, I, Self, ErrRichTokenTree<'a>>
  where
    I: ValueInput<'a, Token = TokenTree, Span = SimpleSpan>,
  {
    let args =
      none_of(Lone(Punct(';'))).map_with_span(id2).repeated().collect::<Vec<_>>();

    Instruction::parser()
      .map_with_span(id2)
      .then(args)
      .then_ignore(semicolon())
      .map(|(name, args)| Self { name, args })
  }
}
impl core::fmt::Debug for InstrUse {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{:?} ", self.name.0)?;
    for (i, x) in self.args.iter().enumerate() {
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
fn test_instr_use_parser() {
  let checks: &[(&str, InstrUse)] = &[
    //
    (
      "ld a, 0;",
      InstrUse {
        name: (Instruction::LD, SimpleSpan::from(0..2)),
        args: vec![
          (Lone(RegA), SimpleSpan::from(3..4)),
          (Lone(Punct(',')), SimpleSpan::from(4..5)),
          (Lone(NumLit("0")), SimpleSpan::from(6..7)),
        ],
      },
    ),
    (
      "cp a, 144;",
      InstrUse {
        name: (Instruction::CP, SimpleSpan::from(0..2)),
        args: vec![
          (Lone(RegA), SimpleSpan::from(3..4)),
          (Lone(Punct(',')), SimpleSpan::from(4..5)),
          (Lone(NumLit("144")), SimpleSpan::from(6..9)),
        ],
      },
    ),
    (
      "inc de;",
      InstrUse {
        name: (Instruction::INC, SimpleSpan::from(0..3)),
        args: vec![(Lone(RegDE), SimpleSpan::from(4..6))],
      },
    ),
    ("daa;", InstrUse { name: (Instruction::DAA, SimpleSpan::from(0..3)), args: vec![] }),
  ];

  for (prog, expected) in checks {
    let tokens = crate::comment_filter::no_comment_tokens(prog).unwrap();
    let token_trees = crate::token_tree::make_token_trees(&tokens).into_output().unwrap();
    let parse_result = run_parser(InstrUse::parser(), &token_trees);
    if parse_result.has_errors() {
      for err in parse_result.errors() {
        println!("ERROR: {err:?}");
      }
      panic!("One or more errors.");
    }
    assert_eq!(expected, parse_result.output().unwrap());
  }
}
