use super::*;

/// A constant declaration.
///
/// Looks like:
/// ```txt
/// const NAME = EXPR;
/// ```
#[derive(Debug, Clone)]
pub struct ConstDecl {
  pub name: (StaticStr, SimpleSpan),
  pub expr: (Vec<(TokenTree, SimpleSpan)>, SimpleSpan),
}
impl ConstDecl {
  pub fn parser<'a, I>() -> impl Parser<'a, I, Self, ErrRichTokenTree<'a>>
  where
    I: ValueInput<'a, Token = TokenTree, Span = SimpleSpan>,
  {
    let kw_const = just(Lone(KwConst));

    let ident = select! {
      Lone(Ident(i)) => i,
    };

    let equal = just(Lone(Punct('=')));

    let expr =
      none_of(Lone(Punct(';'))).map_with_span(spanned).repeated().collect::<Vec<_>>();

    let semicolon = just(Lone(Punct(';')));

    kw_const
      .ignore_then(ident.map_with_span(spanned))
      .then_ignore(equal)
      .then(expr.map_with_span(spanned))
      .then_ignore(semicolon)
      .map(|(name, expr)| ConstDecl { name, expr })
  }
}
