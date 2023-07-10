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
#[derive(Clone, Debug)]
pub struct SectionDecl {
  pub name: (StaticStr, SimpleSpan),
  pub location_tokens: (Vec<(TokenTree, SimpleSpan)>, SimpleSpan),
  pub block_tokens: (Vec<(TokenTree, SimpleSpan)>, SimpleSpan),
}
impl SectionDecl {
  pub fn parser<'a, I>() -> impl Parser<'a, I, Self, ErrRichTokenTree<'a>>
  where
    I: ValueInput<'a, Token = TokenTree, Span = SimpleSpan>,
  {
    let kw_section = just(Lone(KwSection));

    let locations = select! {
      Brackets(l) => l,
    };

    let block = select! {
      Braces(b) => b,
    };

    kw_section
      .ignore_then(ident_parser().map_with_span(id2))
      .then(locations.map_with_span(id2))
      .then(block.map_with_span(id2))
      .map(|((name, location_tokens), block_tokens)| Self {
        name,
        location_tokens,
        block_tokens,
      })
  }
}
