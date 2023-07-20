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
  /// A const declaration is `const` and then some non-`;` stuff, then `;`
  ///
  /// * The "good" path is `const IDENT = CONST_EXPR ;`
  /// * There's all sorts of ways this can go wrong of course.
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
    kw_const.ignore_then(post_keyword)
  }
}
