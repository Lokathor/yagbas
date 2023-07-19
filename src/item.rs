use crate::const_expr::ConstExpr;

use super::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Item {
  ConstDecl(ConstDecl),
  ItemError,
}
impl Item {
  pub fn parser<'a>(
  ) -> impl Parser<'a, TokenTreeSlice<'a>, Self, ErrRichTokenTree<'a>> + Clone {
    let const_decl = ConstDecl::parser().map(Self::ConstDecl);
    let item_error =
      none_of([Lone(Punct(';'))]).repeated().then(semicolon()).to(Self::ItemError);

    choice((const_decl,)).recover_with(via_parser(item_error))
  }
}

/// A const declaration assigns a name to a specific const expression.
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
    let expr = ConstExpr::parser().map_with_span(id2);
    //
    kw_const
      .ignore_then(ident)
      .then_ignore(equal())
      .then(expr)
      .then_ignore(semicolon())
      .map(|(name, expr)| Self { name, expr })
  }
}
