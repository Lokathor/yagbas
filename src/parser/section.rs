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
#[derive(Debug, Clone)]
pub struct Section {
  pub name: (StaticStr, SimpleSpan),
  pub locations: (Vec<(TokenTree, SimpleSpan)>, SimpleSpan),
  pub block: (Vec<(TokenTree, SimpleSpan)>, SimpleSpan),
}
impl Section {
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
      .map(|((name, locations), block)| Self { name, locations, block })
  }
}
