use super::*;

/// A section of code
///
/// Looks like:
/// ```txt
/// section NAME [LOCATIONS] {
///   // labels that end with `:`
///   // statements that end with `;`
/// }
/// ```
#[derive(Clone)]
pub struct SectionDecl {
  pub name: (StaticStr, SimpleSpan),
  pub location_tokens: (Vec<(TokenTree, SimpleSpan)>, SimpleSpan),
  pub block_tokens: (Vec<(TokenTree, SimpleSpan)>, SimpleSpan),
}
impl core::fmt::Debug for SectionDecl {
  /// this cuts out some of the SimpleSpan junk from a debug print compared to
  /// using the derive.
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_struct("SectionDecl")
      .field("name", &self.name.0)
      .field("location_tokens", &DebugListWithoutSpans(&self.location_tokens.0))
      .field("block_tokens", &DebugListWithoutSpans(&self.block_tokens.0))
      .finish()
  }
}
impl SectionDecl {
  pub fn parser<'a, I>() -> impl Parser<'a, I, Self, ErrRichTokenTree<'a>>
  where
    I: ValueInput<'a, Token = TokenTree, Span = SimpleSpan>,
  {
    let kw_section = just(Lone(KwSection));

    let ident = select! {
      Lone(Ident(i)) => i,
    };

    let locations = select! {
      Brackets(l) => l,
    };

    let block = select! {
      Braces(b) => b,
    };

    kw_section
      .ignore_then(ident.map_with_span(id2))
      .then(locations.map_with_span(id2))
      .then(block.map_with_span(id2))
      .map(|((name, locations), block)| Self {
        name,
        location_tokens: locations,
        block_tokens: block,
      })
  }
}
