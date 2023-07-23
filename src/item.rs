use crate::const_expr::ConstExpr;

use super::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Item {
  ConstDecl(ConstDecl),
  StaticDecl(StaticDecl),
  ItemError,
}
impl Item {
  pub fn parser<'a>(
  ) -> impl Parser<'a, TokenTreeSlice<'a>, Self, ErrRichTokenTree<'a>> + Clone {
    let const_decl = ConstDecl::parser().map(Self::ConstDecl);

    let static_decl = StaticDecl::parser().map(Self::StaticDecl);

    let item_error = none_of([Lone(Punct(';'))])
      .ignored()
      .repeated()
      .at_least(1)
      .then(semicolon().ignored().or(end()))
      .to(Self::ItemError)
      .or(semicolon().to(Self::ItemError));

    choice((const_decl, static_decl)).recover_with(via_parser(item_error))
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
      .clone()
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
      .clone()
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

/// An expression that evaluates to 0 or more bytes of static data.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StaticExpr {
  RawBytes(Vec<(ConstExpr, SimpleSpan)>),
  StaticExprError,
}
impl StaticExpr {
  pub fn parser<'a>(
  ) -> impl Parser<'a, TokenTreeSlice<'a>, Self, ErrRichTokenTree<'a>> + Clone {
    let raw_bytes = Self::raw_bytes();
    let generic = Self::generic_eat_to_semicolon();

    choice((raw_bytes,)).recover_with(via_parser(generic))
  }

  fn raw_bytes<'a>(
  ) -> impl Parser<'a, TokenTreeSlice<'a>, Self, ErrRichTokenTree<'a>> + Clone {
    let name = just(Lone(Ident("raw_bytes"))).ignored();
    let bang = bang().ignored();
    let bytes = ConstExpr::parser()
      .map_with_span(id2)
      .separated_by(comma())
      .collect::<Vec<_>>()
      .then_ignore(comma().or_not())
      .nested_in(select_ref! {
        Parens(tokens) = span => {
          let span: SimpleSpan = span;
          tokens.spanned(span)
        },
      });
    name.ignore_then(bang).ignore_then(bytes).map(Self::RawBytes)
  }

  fn generic_eat_to_semicolon<'a>(
  ) -> impl Parser<'a, TokenTreeSlice<'a>, Self, ErrRichTokenTree<'a>> + Clone {
    none_of([Lone(Punct(';'))]).ignored().or(end()).repeated().to(Self::StaticExprError)
  }
}
