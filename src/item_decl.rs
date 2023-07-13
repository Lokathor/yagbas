use crate::{
  const_decl::ConstDecl, instr_use::InstrUse, instruction::Instruction, label::Label,
  macro_use::MacroUse, place::Place, place16::Place16, place8::Place8,
  place_const::PlaceConst, place_indirect::PlaceIndirect, place_use::PlaceUse,
  section_decl::SectionDecl, *,
};

/// An item "declaration" has the minimum outline of an item.
///
/// To make the parsing as recoverable as possible, we only parse for the
/// minimum outline, and leave any more complicated parsing for the next phase
/// of the process.
#[derive(Clone, PartialEq, Eq)]
pub enum ItemDecl {
  ConstDecl(ConstDecl),
  SectionDecl(SectionDecl),
}
impl ItemDecl {
  pub fn parser<'a, I>() -> impl Parser<'a, I, Self, ErrRichTokenTree<'a>>
  where
    I: ValueInput<'a, Token = TokenTree, Span = SimpleSpan>,
  {
    let const_decl = ConstDecl::parser().map(Self::ConstDecl);
    let section_decl = SectionDecl::parser().map(Self::SectionDecl);
    choice((const_decl, section_decl))
  }
}
impl core::fmt::Debug for ItemDecl {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Self::ConstDecl(x) => write!(f, "{x:?}"),
      Self::SectionDecl(x) => write!(f, "{x:?}"),
    }
  }
}

#[test]
fn test_parser() {
  let checks: &[(&str, ItemDecl)] = &[
    (
      "const FOO = 10;",
      ItemDecl::ConstDecl(ConstDecl {
        name: ("FOO", SimpleSpan::from(6..9)),
        expr: vec![(Lone(NumLit("10")), SimpleSpan::from(12..14))],
      }),
    ),
    (
      "section main [rom0] { nop; }",
      ItemDecl::SectionDecl(SectionDecl {
        name: ("main", SimpleSpan::from(8..12)),
        location: vec![(Lone(Ident("rom0")), SimpleSpan::from(14..18))],
        block: vec![
          (Lone(InstNOP), SimpleSpan::from(22..25)),
          (Lone(Punct(';')), SimpleSpan::from(25..26)),
        ],
      }),
    ),
  ];

  for (prog, expected) in checks {
    let tokens = crate::comment_filter::no_comment_tokens(prog).unwrap();
    let token_trees = crate::token_tree::make_token_trees(&tokens).into_output().unwrap();
    let parse_result = run_parser(ItemDecl::parser(), &token_trees);
    if parse_result.has_errors() {
      for err in parse_result.errors() {
        println!("ERROR: {err:?}");
      }
      panic!("One or more errors.");
    }
    assert_eq!(expected, parse_result.output().unwrap());
  }
}
