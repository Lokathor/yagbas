use crate::const_expr::ConstExpr;

use super::*;

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
    let recovery = eat_until_parser(semicolon()).to(Self::StaticExprError);

    choice((raw_bytes,)).recover_with(via_parser(recovery))
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
}
