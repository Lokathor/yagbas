use crate::{ast::Ast, *};

#[derive(Clone, PartialEq, Eq)]
pub struct MacroUse {
  pub name: (StaticStr, SimpleSpan),
  pub args: Vec<(TokenTree, SimpleSpan)>,
}
impl MacroUse {
  pub fn parser<'a, I>() -> impl Parser<'a, I, Self, ErrRichTokenTree<'a>> + Clone
  where
    I: ValueInput<'a, Token = TokenTree, Span = SimpleSpan>,
  {
    ident()
      .map_with_span(id2)
      .then_ignore(bang())
      .then(paren_group())
      .map(|(name, args)| Self { name, args })
  }
}
impl core::fmt::Debug for MacroUse {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}!(", self.name.0)?;
    for (i, x) in self.args.iter().enumerate() {
      if i > 0 {
        write!(f, " ")?;
      }
      write!(f, "{x:?}")?;
    }
    write!(f, ")")?;
    Ok(())
  }
}

#[test]
fn test_parser() {
  let checks: &[(&str, MacroUse)] = &[
    //
    ("check!()", MacroUse { name: ("check", SimpleSpan::from(0..5)), args: vec![] }),
    (
      "zero_bytes!(5)",
      MacroUse {
        name: ("zero_bytes", SimpleSpan::from(0..10)),
        args: vec![(Lone(NumLit("5")), SimpleSpan::from(12..13))],
      },
    ),
    (
      "raw_bytes!($00,$F0)",
      MacroUse {
        name: ("raw_bytes", SimpleSpan::from(0..9)),
        args: vec![
          (Lone(NumLit("$00")), SimpleSpan::from(11..14)),
          (Lone(Punct(',')), SimpleSpan::from(14..15)),
          (Lone(NumLit("$F0")), SimpleSpan::from(15..18)),
        ],
      },
    ),
    (
      "size_of_section!(tiles)",
      MacroUse {
        name: ("size_of_section", SimpleSpan::from(0..15)),
        args: vec![(Lone(Ident("tiles")), SimpleSpan::from(17..22))],
      },
    ),
  ];

  for (prog, expected) in checks {
    let tokens = Ast::tokenize(prog.to_string());
    let token_trees =
      crate::token_tree::make_token_trees(&tokens.items).into_output().unwrap();
    let parse_result = run_parser(MacroUse::parser(), &token_trees);
    if parse_result.has_errors() {
      for err in parse_result.errors() {
        println!("ERROR: {err:?}");
      }
      panic!("One or more errors.");
    }
    assert_eq!(expected, parse_result.output().unwrap());
  }
}
