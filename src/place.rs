use crate::{
  place16::Place16, place8::Place8, place_const::PlaceConst,
  place_indirect::PlaceIndirect, *,
};

#[derive(Clone, PartialEq, Eq)]
pub enum Place {
  Place8(Place8),
  Place16(Place16),
  PlaceIndirect(PlaceIndirect),
  PlaceConst(PlaceConst),
}
impl Place {
  pub fn parser<'a, I>() -> impl Parser<'a, I, Self, ErrRichTokenTree<'a>>
  where
    I: ValueInput<'a, Token = TokenTree, Span = SimpleSpan>,
  {
    let eight = Place8::parser().map(Self::Place8);
    let sixteen = Place16::parser().map(Self::Place16);
    let indirect = PlaceIndirect::parser().map(Self::PlaceIndirect);
    let const_ = PlaceConst::parser().map(Self::PlaceConst);
    choice((eight, sixteen, indirect, const_))
  }
}
impl core::fmt::Debug for Place {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Self::Place8(x) => write!(f, "{x:?}"),
      Self::Place16(x) => write!(f, "{x:?}"),
      Self::PlaceIndirect(x) => write!(f, "{x:?}"),
      Self::PlaceConst(x) => write!(f, "{x:?}"),
    }
  }
}

#[test]
fn test_place_parser() {
  let checks: &[(&str, Place)] = &[
    ("a", Place::Place8(Place8::A)),
    ("b", Place::Place8(Place8::B)),
    ("c", Place::Place8(Place8::C)),
    ("d", Place::Place8(Place8::D)),
    ("e", Place::Place8(Place8::E)),
    ("h", Place::Place8(Place8::H)),
    ("l", Place::Place8(Place8::L)),
    ("[hl]", Place::Place8(Place8::AddrHL)),
    //
    ("bc", Place::Place16(Place16::BC)),
    ("de", Place::Place16(Place16::DE)),
    ("hl", Place::Place16(Place16::HL)),
    ("sp", Place::Place16(Place16::SP)),
    //
    ("[bc]", Place::PlaceIndirect(PlaceIndirect::AddrBC)),
    ("[de]", Place::PlaceIndirect(PlaceIndirect::AddrDE)),
    ("[hl++]", Place::PlaceIndirect(PlaceIndirect::AddrHLInc)),
    ("[hl--]", Place::PlaceIndirect(PlaceIndirect::AddrHLDec)),
    ("[hl+]", Place::PlaceIndirect(PlaceIndirect::AddrHLInc)),
    ("[hl-]", Place::PlaceIndirect(PlaceIndirect::AddrHLDec)),
    //
    (
      "[FOO]",
      Place::PlaceConst(PlaceConst(vec![(Lone(Ident("FOO")), SimpleSpan::from(1..4))])),
    ),
    (
      "[$FF00]",
      Place::PlaceConst(PlaceConst(vec![(
        Lone(NumLit("$FF00")),
        SimpleSpan::from(1..6),
      )])),
    ),
    (
      "[1 + 2]",
      Place::PlaceConst(PlaceConst(vec![
        (Lone(NumLit("1")), SimpleSpan::from(1..2)),
        (Lone(Punct('+')), SimpleSpan::from(3..4)),
        (Lone(NumLit("2")), SimpleSpan::from(5..6)),
      ])),
    ),
  ];

  for (prog, expected) in checks {
    let tokens = crate::comment_filter::no_comment_tokens(prog).unwrap();
    let token_trees = crate::token_tree::make_token_trees(&tokens).into_output().unwrap();
    let parse_result = run_parser(Place::parser(), &token_trees);
    if parse_result.has_errors() {
      for err in parse_result.errors() {
        println!("ERROR: {err:?}");
      }
      panic!("One or more errors.");
    }
    assert_eq!(expected, parse_result.output().unwrap());
  }
}
