use super::*;

#[derive(Clone)]
pub struct StatementDecl {
  tokens: Vec<(TokenTree, SimpleSpan)>,
}
impl core::fmt::Debug for StatementDecl {
  /// this cuts out some of the SimpleSpan junk from a debug print compared to
  /// using the derive.
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    for (i, (tt, _span)) in self.tokens.iter().enumerate() {
      if i > 0 {
        write!(f, ", ")?;
      }
      write!(f, "{tt:?}")?;
    }
    write!(f, ";")
  }
}
impl StatementDecl {
  pub fn parser<'a, I>() -> impl Parser<'a, I, Self, ErrRichTokenTree<'a>>
  where
    I: ValueInput<'a, Token = TokenTree, Span = SimpleSpan>,
  {
    none_of([Lone(Punct(';'))])
      .map_with_span(id2)
      .repeated()
      .collect::<Vec<_>>()
      .map(|tokens| Self { tokens })
      .then_ignore(just(Lone(Punct(';'))))
  }
}
