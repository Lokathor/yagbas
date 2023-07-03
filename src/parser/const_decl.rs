use super::*;

/// A constant declaration.
///
/// Looks like:
/// ```txt
/// const NAME = EXPR;
/// ```
#[derive(Clone)]
pub struct ConstDecl {
  pub name: (StaticStr, SimpleSpan),
  pub expr_tokens: (Vec<(TokenTree, SimpleSpan)>, SimpleSpan),
}
impl core::fmt::Debug for ConstDecl {
  /// this cuts out some of the SimpleSpan junk from a debug print compared to
  /// using the derive.
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_struct("ConstDecl")
      .field("name", &self.name.0)
      .field("expr_tokens", &DebugListWithoutSpans(&self.expr_tokens.0))
      .finish()
  }
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
      none_of(Lone(Punct(';'))).map_with_span(id2).repeated().collect::<Vec<_>>();

    let semicolon = just(Lone(Punct(';')));

    kw_const
      .ignore_then(ident.map_with_span(id2))
      .then_ignore(equal)
      .then(expr.map_with_span(id2))
      .then_ignore(semicolon)
      .map(|(name, expr)| ConstDecl { name, expr_tokens: expr })
  }
}
