use crate::{
  ast::Ast, cond::Cond, const_expr::ConstExpr, inst::Inst, jump_target::JumpTarget,
  place16::Place16, place8::Place8, place_indirect::PlaceIndirect, *,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InstUse {
  LoadPlace8ConstExpr { place: Place8, expr: ConstExpr },
  LoadConstAddrA { addr: ConstExpr },
  LoadAConstAddr { addr: ConstExpr },
  LoadAPlaceIndirect { place: PlaceIndirect },
  LoadPlace8Place8 { dst: Place8, src: Place8 },
  LoadPlaceIndirectA { place: PlaceIndirect },
  CompareAConst { expr: ConstExpr },
  OrAPlace8(Place8),
  Inc16(Place16),
  Dec16(Place16),
  Jump { cond: Option<Cond>, target: JumpTarget },
  LoadPlace16ConstExpr { place: Place16, expr: ConstExpr },
  UNKNOWN { name: (Inst, SimpleSpan), args: Vec<(TokenTree, SimpleSpan)> },
}
impl InstUse {
  pub fn parser<'a>() -> impl Parser<'a, TokenTreeInput<'a>, Self, ErrRichTokenTree<'a>> {
    let ld = select! {
      Lone(InstLD) => (),
    };
    let cp = select! {
      Lone(InstCP) => (),
    };
    let jp = select! {
      Lone(InstJP) => (),
    };
    let inc = select! {
      Lone(InstINC) => (),
    };
    let dec = select! {
      Lone(InstDEC) => (),
    };
    let or = select! {
      Lone(InstOR) => (),
    };
    let a = just(Lone(RegA));

    let load_place8_const_expr = ld
      .ignore_then(Place8::parser())
      .then_ignore(comma())
      .then(ConstExpr::parser())
      .then_ignore(semicolon())
      .map(|(place, expr)| Self::LoadPlace8ConstExpr { place, expr });
    let load_place16_const_expr = ld
      .ignore_then(Place16::parser())
      .then_ignore(comma())
      .then(ConstExpr::parser())
      .then_ignore(semicolon())
      .map(|(place, expr)| Self::LoadPlace16ConstExpr { place, expr });
    let load_place8_place8 = ld
      .ignore_then(Place8::parser())
      .then_ignore(comma())
      .then(Place8::parser())
      .then_ignore(semicolon())
      .map(|(dst, src)| Self::LoadPlace8Place8 { dst, src });

    let inc16 =
      inc.ignore_then(Place16::parser()).then_ignore(semicolon()).map(Self::Inc16);
    let dec16 =
      dec.ignore_then(Place16::parser()).then_ignore(semicolon()).map(Self::Dec16);
    let or_a_place8 = or
      .ignore_then(a.clone())
      .ignore_then(comma())
      .ignore_then(Place8::parser())
      .then_ignore(semicolon())
      .map(Self::OrAPlace8);

    let const_addr_expr = ConstExpr::parser()
      .nested_in(select_ref! {
          Brackets(tokens) = span => {
              let span: SimpleSpan = span;
              tokens[..].spanned(SimpleSpan::from(span.end..span.end))
          },
      })
      .recover_with(via_parser(select! { Brackets(_) => ConstExpr::ConstExprError }));
    let load_const_addr_a = ld
      .ignore_then(const_addr_expr.clone())
      .then_ignore(comma())
      .then_ignore(a.clone())
      .then_ignore(semicolon())
      .map(|addr| Self::LoadConstAddrA { addr });
    let load_a_const_addr = ld
      .ignore_then(a.clone())
      .ignore_then(comma())
      .ignore_then(const_addr_expr)
      .then_ignore(semicolon())
      .map(|addr| Self::LoadConstAddrA { addr });
    let compare_a_const = cp
      .ignore_then(a.clone())
      .ignore_then(comma())
      .ignore_then(ConstExpr::parser())
      .then_ignore(semicolon())
      .map(|expr| Self::CompareAConst { expr });
    let load_a_place_indirect = ld
      .ignore_then(a.clone())
      .ignore_then(comma())
      .ignore_then(PlaceIndirect::parser())
      .then_ignore(semicolon())
      .map(|place| Self::LoadAPlaceIndirect { place });
    let load_place_indirect_a = ld
      .ignore_then(PlaceIndirect::parser())
      .then_ignore(comma())
      .then_ignore(a.clone())
      .then_ignore(semicolon())
      .map(|place| Self::LoadPlaceIndirectA { place });

    let jump = jp
      .ignore_then(Cond::parser().then_ignore(comma()).or_not())
      .then(JumpTarget::parser())
      .then_ignore(semicolon())
      .map(|(cond, target)| Self::Jump { cond, target });

    let unknown_args =
      none_of(Lone(Punct(';'))).map_with_span(id2).repeated().collect::<Vec<_>>();
    let unknown_fallback = Inst::parser()
      .map_with_span(id2)
      .then(unknown_args)
      .then_ignore(semicolon())
      .map(|(name, args)| Self::UNKNOWN { name, args });

    // Note: Order doesn't matter (or shouldn't matter!) as long as the
    // `unknown_fallback` is the last one we try
    choice((
      inc16,
      dec16,
      or_a_place8,
      load_place8_place8,
      load_const_addr_a,
      load_place8_const_expr,
      load_place16_const_expr,
      load_a_const_addr,
      load_a_place_indirect,
      load_place_indirect_a,
      compare_a_const,
      jump,
    ))
    .recover_with(via_parser(unknown_fallback))
  }
}

#[test]
fn test_parser() {
  let checks: &[(&str, InstUse)] = &[
    //
    (
      "ld a, 0;",
      InstUse::LoadPlace8ConstExpr { place: Place8::A, expr: ConstExpr::Value(0) },
    ),
    ("cp a, 144;", InstUse::CompareAConst { expr: ConstExpr::Value(144) }),
    ("inc de;", InstUse::Inc16(Place16::DE)),
    (
      "daa;",
      InstUse::UNKNOWN { name: (Inst::DAA, SimpleSpan::from(0..3)), args: vec![] },
    ),
  ];

  for (prog, expected) in checks {
    let tokens = Ast::tokenize(prog.to_string());
    let token_trees = tokens.into_token_trees();
    let parse_result = run_parser(InstUse::parser(), &token_trees.items);
    if parse_result.has_errors() {
      for err in parse_result.errors() {
        println!("ERROR: {err:?}");
      }
    }
    assert_eq!(expected, parse_result.output().unwrap());
  }
}
