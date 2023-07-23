use crate::{
  const_expr::{lit_to_value, ConstExpr},
  static_expr::StaticExpr,
};

use super::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Item {
  ConstDecl(ConstDecl),
  StaticDecl(StaticDecl),
  SectionDecl(SectionDecl),
  ItemError,
}
impl Item {
  pub fn parser<'a>(
  ) -> impl Parser<'a, TokenTreeSlice<'a>, Self, ErrRichTokenTree<'a>> + Clone {
    let const_decl = ConstDecl::parser().map(Self::ConstDecl);

    let static_decl = StaticDecl::parser().map(Self::StaticDecl);

    let section_decl = SectionDecl::parser().map(Self::SectionDecl);

    let item_error = eat_until_semicolon_or_braces().to(Self::ItemError);

    choice((const_decl, static_decl, section_decl)).recover_with(via_parser(item_error))
  }
}

pub fn parse_module_items(
  trees: &[(TokenTree, SimpleSpan)],
) -> (Vec<(Item, SimpleSpan)>, Vec<Rich<'static, TokenTree>>) {
  let len = trees.last().map(|(_, s)| s.end).unwrap_or(0);
  let span: SimpleSpan = SimpleSpan::from(len..len);
  let parser = Item::parser().map_with_span(id2).repeated().collect::<Vec<_>>();
  let input = trees.spanned(span);
  let (items, errors) = parser.parse(input).into_output_errors();
  (
    items.unwrap_or_default(),
    errors.into_iter().map(|r| r.into_owned()).collect::<Vec<_>>(),
  )
}

/// A const declaration assigns a name to a specific const expression.
///
/// A const declaration is `const` and then some non-`;` stuff, then `;`
///
/// * Should look like: `const IDENT = CONST_EXPR ;`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstDecl {
  pub name: (StaticStr, SimpleSpan),
  pub expr: (ConstExpr, SimpleSpan),
}
impl ConstDecl {
  pub fn parser<'a>(
  ) -> impl Parser<'a, TokenTreeSlice<'a>, Self, ErrRichTokenTree<'a>> + Clone {
    let kw_const = just(Lone(KwConst));
    let ident = ident().map_with_span(id2);
    let not_semicolon = none_of([Lone(Punct(';'))]);
    let expr = ConstExpr::parser().map_with_span(id2).recover_with(via_parser(
      not_semicolon
        .clone()
        .ignored()
        .repeated()
        .map_with_span(|(), span| (ConstExpr::BAD_PARSE, span)),
    ));

    // the "good" path of what we want after the keyword.
    let all_parts = ident
      .then_ignore(equal())
      .then(expr.clone())
      .then_ignore(semicolon())
      .map(|(name, expr)| Self { name, expr });

    // the simplest error recovery for any bad const declaration is to just eat
    // everything up to and including the `;` at the end of this const
    // declaration. This also doesn't give any useful information to the user
    // about what went wrong, so we should improve this strategy later.
    let generic_eat_to_end = not_semicolon
      .clone()
      .ignored()
      .repeated()
      .then_ignore(semicolon().ignored().or(end()))
      .map_with_span(|(), span| Self {
        name: ("", span),
        expr: (ConstExpr::BAD_PARSE, span),
      });

    // now assemble how we parse everything *after* the keyword.
    let post_keyword = all_parts.recover_with(via_parser(generic_eat_to_end));

    // If we see `const` we run the rest and whatever happens it's still some
    // sort of const declaration (since we saw the keyword). If we don't even
    // see `const` then it's not a const declaration at all.
    kw_const.ignore_then(post_keyword).boxed()
  }
}

/// A static declaration is `static` and then some non-`;` stuff, then `;`
///
/// * Should look like: `static IDENT [LOCATIONS] = STATIC_EXPR ;`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StaticDecl {
  pub name: (StaticStr, SimpleSpan),
  pub rom_locations: Vec<(RomLocation, SimpleSpan)>,
  pub expr: (StaticExpr, SimpleSpan),
}
impl StaticDecl {
  pub fn parser<'a>(
  ) -> impl Parser<'a, TokenTreeSlice<'a>, Self, ErrRichTokenTree<'a>> + Clone {
    let kw_static = just(Lone(KwStatic));
    let ident = ident().map_with_span(id2);
    let not_semicolon = none_of([Lone(Punct(';'))]);
    let expr = StaticExpr::parser().map_with_span(id2).recover_with(via_parser(
      not_semicolon
        .clone()
        .ignored()
        .repeated()
        .map_with_span(|(), span| (StaticExpr::StaticExprError, span)),
    ));
    let locations = RomLocation::parser()
      .map_with_span(id2)
      .separated_by(comma())
      .collect::<Vec<_>>()
      .nested_in(select_ref! {
        Brackets(tokens) = span => {
          let span: SimpleSpan = span;
          tokens.spanned(span)
        },
      });
    let all_parts = ident
      .then(locations)
      .then_ignore(equal())
      .then(expr.clone())
      .then_ignore(semicolon())
      .map(|((name, rom_locations), expr)| Self { name, rom_locations, expr });
    let generic_eat_to_end = not_semicolon
      .clone()
      .ignored()
      .repeated()
      .then_ignore(semicolon().ignored().or(end()))
      .map_with_span(|(), span| Self {
        name: ("", span),
        expr: (StaticExpr::StaticExprError, span),
        rom_locations: vec![],
      });
    let post_keyword = all_parts.recover_with(via_parser(generic_eat_to_end));

    kw_static.ignore_then(post_keyword).boxed()
  }
}

/// A location within the ROM output
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RomLocation {
  Rom0,
}
impl RomLocation {
  pub fn parser<'a>(
  ) -> impl Parser<'a, TokenTreeSlice<'a>, Self, ErrRichTokenTree<'a>> + Clone {
    let rom0 = just(Lone(Ident("rom0"))).to(Self::Rom0);

    rom0
  }
}

/// A static declaration is `section` and then some non-braces stuff, then
/// braces.
///
/// * Should look like: `static IDENT [LOCATIONS] { STATEMENTS }`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SectionDecl {
  pub name: (StaticStr, SimpleSpan),
  pub rom_locations: Vec<(RomLocation, SimpleSpan)>,
  pub elements: Vec<(SectionElem, SimpleSpan)>,
}
impl SectionDecl {
  pub fn parser<'a>(
  ) -> impl Parser<'a, TokenTreeSlice<'a>, Self, ErrRichTokenTree<'a>> + Clone {
    let kw_section = just(Lone(KwSection));

    let ident = ident().map_with_span(id2);

    let locations = RomLocation::parser()
      .map_with_span(id2)
      .separated_by(comma())
      .collect::<Vec<_>>()
      .nested_in(select_ref! {
        Brackets(tokens) = span => {
          let span: SimpleSpan = span;
          tokens.spanned(span)
        },
      });

    let all_parts = ident
      .then(locations)
      .then(
        SectionElem::parser()
          .map_with_span(id2)
          .repeated()
          .collect::<Vec<_>>()
          .nested_in(select_ref! {
            Braces(tokens) = span => {
              let span: SimpleSpan = span;
              tokens.spanned(span)
            },
          }),
      )
      .map(|((name, rom_locations), elements)| Self { name, rom_locations, elements });

    let recovery = eat_until_semicolon_or_braces().to(Self {
      name: ("", SimpleSpan::new(0, 0)),
      rom_locations: Default::default(),
      elements: Default::default(),
    });

    let post_keyword = all_parts.recover_with(via_parser(recovery));

    kw_section.ignore_then(post_keyword).boxed()
  }
}

/// A statement within a section.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SectionElem {
  NumberLabel(i32),
  IdentLabel(StaticStr),
  Instruction(Instruction),
  SectionElemError(CowStr),
}
impl SectionElem {
  pub fn parser<'a>(
  ) -> impl Parser<'a, TokenTreeSlice<'a>, Self, ErrRichTokenTree<'a>> + Clone {
    let number_label = num_lit().then_ignore(colon()).map(|n| match lit_to_value(n) {
      Ok(i) => Self::NumberLabel(i),
      Err(e) => Self::SectionElemError(e),
    });

    let ident_label = ident().then_ignore(colon()).map(Self::IdentLabel);

    let instruction = Instruction::parser().map(Self::Instruction);

    let happy = choice((number_label, ident_label, instruction));

    let recovery =
      eat_until_semicolon_or_braces().to(Self::SectionElemError(CowStr::Borrowed("")));

    happy.recover_with(via_parser(recovery))
  }
}

/// A particular instruction.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction {
  Load(Load),
  Compare(Compare),
  Jump(Jump),
  InstructionError,
}
impl Instruction {
  pub fn parser<'a>(
  ) -> impl Parser<'a, TokenTreeSlice<'a>, Self, ErrRichTokenTree<'a>> + Clone {
    let load = Load::parser().map(Self::Load);
    let compare = Compare::parser().map(Self::Compare);
    let jump = Jump::parser().map(Self::Jump);

    let happy = choice((load, compare, jump));

    let recovery = eat_until_semicolon_or_braces().to(Self::InstructionError);

    happy.recover_with(via_parser(recovery))
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[rustfmt::skip]
pub enum Place8 {
  B,C,D,E,H,L,AddrHL,A
}
impl Place8 {
  pub fn parser<'a>(
  ) -> impl Parser<'a, TokenTreeSlice<'a>, Self, ErrRichTokenTree<'a>> + Clone + Copy {
    let lone = select! {
      Lone(RegA) => Self::A,
      Lone(RegB) => Self::B,
      Lone(RegC) => Self::C,
      Lone(RegD) => Self::D,
      Lone(RegE) => Self::E,
      Lone(RegH) => Self::H,
      Lone(RegL) => Self::L,
    };
    let hl = select! {
      Lone(RegHL) => Self::AddrHL
    }
    .nested_in(select_ref! {
      Brackets(tokens) = span => {
        let span: SimpleSpan = span;
        tokens.spanned(span)
      },
    });
    lone.or(hl).labelled("Place8")
  }
}

/// A statement within a section.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Load {
  Place8Imm8((Place8, SimpleSpan), (ConstExpr, SimpleSpan)),
  ConstAddrA((ConstExpr, SimpleSpan)),
  AConstAddr((ConstExpr, SimpleSpan)),
  LoadError,
}
impl Load {
  pub fn parser<'a>(
  ) -> impl Parser<'a, TokenTreeSlice<'a>, Self, ErrRichTokenTree<'a>> + Clone {
    let ld = select! {
      Lone(InstLD) => (),
    };
    let a = select! {
      Lone(RegA) => (),
    };

    let const_addr = ConstExpr::parser()
      .nested_in(select_ref! {
        Brackets(tokens) = span => {
          let span: SimpleSpan = span;
          tokens.spanned(span)
        },
      })
      .labelled("ConstAddress")
      .map_with_span(id2);

    let place8_imm8 = Place8::parser()
      .map_with_span(id2)
      .then_ignore(comma())
      .then(ConstExpr::parser().map_with_span(id2))
      .map(|(place, expr)| Self::Place8Imm8(place, expr));

    let const_addr_a =
      const_addr.clone().then_ignore(comma()).then_ignore(a).map(Self::ConstAddrA);
    let a_const_addr =
      a.ignore_then(comma()).ignore_then(const_addr.clone()).map(Self::AConstAddr);

    let happy =
      choice((place8_imm8, const_addr_a, a_const_addr)).then_ignore(semicolon());
    let recovery = via_parser(eat_until_semicolon_or_braces().to(Self::LoadError));

    ld.ignore_then(happy.recover_with(recovery))
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Compare {
  Place8(Place8, SimpleSpan),
  ConstExpr(ConstExpr, SimpleSpan),
  CompareError,
}
impl Compare {
  pub fn parser<'a>(
  ) -> impl Parser<'a, TokenTreeSlice<'a>, Self, ErrRichTokenTree<'a>> + Clone {
    let cp = select! {
      Lone(InstCP) => (),
    };
    let a = select! {
      Lone(RegA) => (),
    };

    let a_place8 =
      a.ignore_then(comma()).ignore_then(Place8::parser().map_with_span(Self::Place8));
    let a_const = a
      .ignore_then(comma())
      .ignore_then(ConstExpr::parser().map_with_span(Self::ConstExpr));
    let only_place8 = Place8::parser().map_with_span(Self::Place8);
    let only_const = ConstExpr::parser().map_with_span(Self::ConstExpr);

    let happy =
      choice((a_place8, a_const, only_place8, only_const)).then_ignore(semicolon());
    let recovery = via_parser(eat_until_semicolon_or_braces().to(Self::CompareError));

    cp.ignore_then(happy.recover_with(recovery))
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[rustfmt::skip]
pub enum Condition {
  Zero, NonZero, Carry, NoCarry, Always
}
impl Condition {
  pub fn parser<'a>(
  ) -> impl Parser<'a, TokenTreeSlice<'a>, Self, ErrRichTokenTree<'a>> + Clone + Copy {
    select! {
      Lone(CondZE) => Self::Zero,
      Lone(CondNZ) => Self::NonZero,
      Lone(CondCY) => Self::Carry,
      Lone(CondNC) => Self::NoCarry,
      Lone(CondAL) => Self::Always,
    }
    .labelled("Condition")
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum JumpDirection {
  Back,
  Forward,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NumLabelTarget(i32, JumpDirection);
impl NumLabelTarget {
  pub fn parser<'a>(
  ) -> impl Parser<'a, TokenTreeSlice<'a>, Self, ErrRichTokenTree<'a>> + Clone {
    num_lit()
      .try_map(|lit, span| {
        Ok(if let Some(lit) = lit.strip_suffix('f') {
          NumLabelTarget(
            lit_to_value(lit).map_err(|e| Rich::custom(span, e))?,
            JumpDirection::Forward,
          )
        } else if let Some(lit) = lit.strip_suffix('b') {
          NumLabelTarget(
            lit_to_value(lit).map_err(|e| Rich::custom(span, e))?,
            JumpDirection::Back,
          )
        } else {
          return Err(Rich::custom(span, "Number label requires `b` or `f` suffix"));
        })
      })
      .labelled("NumericLabel")
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Jump {
  NumLabel(Condition, NumLabelTarget),
  JumpError,
}
impl Jump {
  pub fn parser<'a>(
  ) -> impl Parser<'a, TokenTreeSlice<'a>, Self, ErrRichTokenTree<'a>> + Clone {
    let jp = select! {
      Lone(InstJP) => (),
    };

    let cond_num_label = Condition::parser()
      .then_ignore(comma())
      .then(NumLabelTarget::parser())
      .map(|(cond, nlt)| Self::NumLabel(cond, nlt));
    let always_num_label =
      NumLabelTarget::parser().map(|nlt| Self::NumLabel(Condition::Always, nlt));

    let happy = choice((cond_num_label, always_num_label)).then_ignore(semicolon());
    let recovery = via_parser(eat_until_semicolon_or_braces().to(Self::JumpError));

    jp.ignore_then(happy.recover_with(recovery))
  }
}
