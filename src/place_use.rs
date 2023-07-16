use crate::{ast::Ast, place::Place, place16::Place16, place8::Place8, *};

#[derive(Clone, PartialEq, Eq)]
pub struct PlaceUse {
  pub name: (Place, SimpleSpan),
  pub args: Vec<(TokenTree, SimpleSpan)>,
}
impl PlaceUse {
  pub fn parser<'a, I>() -> impl Parser<'a, I, Self, ErrRichTokenTree<'a>>
  where
    I: ValueInput<'a, Token = TokenTree, Span = SimpleSpan>,
  {
    let args =
      none_of(Lone(Punct(';'))).map_with_span(id2).repeated().collect::<Vec<_>>();

    Place::parser()
      .map_with_span(id2)
      .then(args)
      .then_ignore(semicolon())
      .map(|(name, args)| Self { name, args })
  }
}
impl core::fmt::Debug for PlaceUse {
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
  let checks: &[(&str, PlaceUse)] = &[
    //
    (
      "a = 0;",
      PlaceUse {
        name: (Place::Place8(Place8::A), SimpleSpan::from(0..1)),
        args: vec![
          (Lone(Punct('=')), SimpleSpan::from(2..3)),
          (Lone(NumLit("0")), SimpleSpan::from(4..5)),
        ],
      },
    ),
    (
      "a &= 0xF0;",
      PlaceUse {
        name: (Place::Place8(Place8::A), SimpleSpan::from(0..1)),
        args: vec![
          (Lone(Punct('&')), SimpleSpan::from(2..3)),
          (Lone(Punct('=')), SimpleSpan::from(3..4)),
          (Lone(NumLit("0xF0")), SimpleSpan::from(5..9)),
        ],
      },
    ),
    (
      "de++;",
      PlaceUse {
        name: (Place::Place16(Place16::DE), SimpleSpan::from(0..2)),
        args: vec![
          (Lone(Punct('+')), SimpleSpan::from(2..3)),
          (Lone(Punct('+')), SimpleSpan::from(3..4)),
        ],
      },
    ),
  ];

  for (prog, expected) in checks {
    let tokens = Ast::tokenize(prog.to_string());
    let token_trees =
      crate::token_tree::make_token_trees(&tokens.items).into_output().unwrap();
    let parse_result = run_parser(PlaceUse::parser(), &token_trees);
    if parse_result.has_errors() {
      for err in parse_result.errors() {
        println!("ERROR: {err:?}");
      }
      panic!("One or more errors.");
    }
    assert_eq!(expected, parse_result.output().unwrap());
  }
}
