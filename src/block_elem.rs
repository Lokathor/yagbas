use crate::{
  instr_use::InstrUse, instruction::Instruction, label::Label, macro_use::MacroUse,
  place::Place, place16::Place16, place8::Place8, place_const::PlaceConst,
  place_indirect::PlaceIndirect, place_use::PlaceUse, *,
};

#[derive(Clone, PartialEq, Eq)]
pub enum BlockElem {
  Label(Label),
  MacroUse(MacroUse),
  InstrUse(InstrUse),
  PlaceUse(PlaceUse),
}
impl BlockElem {
  pub fn parser<'a, I>() -> impl Parser<'a, I, Self, ErrRichTokenTree<'a>>
  where
    I: ValueInput<'a, Token = TokenTree, Span = SimpleSpan>,
  {
    let label = Label::parser().map(Self::Label);
    let macro_ = MacroUse::parser().map(Self::MacroUse).then_ignore(semicolon());
    let instr = InstrUse::parser().map(Self::InstrUse);
    let place = PlaceUse::parser().map(Self::PlaceUse);
    choice((label, macro_, instr, place))
  }
}
impl core::fmt::Debug for BlockElem {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Self::Label(x) => write!(f, "{x:?}"),
      Self::MacroUse(x) => write!(f, "{x:?}"),
      Self::InstrUse(x) => write!(f, "{x:?}"),
      Self::PlaceUse(x) => write!(f, "{x:?}"),
    }
  }
}

#[test]
fn test_block_elem_parser() {
  let checks: &[(&str, BlockElem)] = &[
    ("here:", BlockElem::Label(Label::Ident("here"))),
    (
      "ld a, 0;",
      BlockElem::InstrUse(InstrUse {
        name: (Instruction::LD, SimpleSpan::from(0..2)),
        args: vec![
          (Lone(RegA), SimpleSpan::from(3..4)),
          (Lone(Punct(',')), SimpleSpan::from(4..5)),
          (Lone(NumLit("0")), SimpleSpan::from(6..7)),
        ],
      }),
    ),
    (
      "a &= 0xF0;",
      BlockElem::PlaceUse(PlaceUse {
        name: (Place::Place8(Place8::A), SimpleSpan::from(0..1)),
        args: vec![
          (Lone(Punct('&')), SimpleSpan::from(2..3)),
          (Lone(Punct('=')), SimpleSpan::from(3..4)),
          (Lone(NumLit("0xF0")), SimpleSpan::from(5..9)),
        ],
      }),
    ),
    (
      "raw_bytes!($00,$F0);",
      BlockElem::MacroUse(MacroUse {
        name: ("raw_bytes", SimpleSpan::from(0..9)),
        args: vec![
          (Lone(NumLit("$00")), SimpleSpan::from(11..14)),
          (Lone(Punct(',')), SimpleSpan::from(14..15)),
          (Lone(NumLit("$F0")), SimpleSpan::from(15..18)),
        ],
      }),
    ),
  ];

  for (prog, expected) in checks {
    let tokens = crate::comment_filter::no_comment_tokens(prog).unwrap();
    let token_trees = crate::token_tree::make_token_trees(&tokens).into_output().unwrap();
    let parse_result = run_parser(BlockElem::parser(), &token_trees);
    if parse_result.has_errors() {
      for err in parse_result.errors() {
        println!("ERROR: {err:?}");
      }
      panic!("One or more errors.");
    }
    assert_eq!(expected, parse_result.output().unwrap());
  }
}
