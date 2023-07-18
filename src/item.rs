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

    choice((const_decl,))
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
    kw_const
      .ignore_then(ident().map_with_span(id2))
      .then_ignore(equal())
      .then(ConstExpr::parser().map_with_span(id2))
      .then_ignore(semicolon())
      .map(|(name, expr)| Self { name, expr })
  }
}
