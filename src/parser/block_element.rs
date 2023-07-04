use super::*;

#[derive(Clone)]
pub enum BlockElement {
  Label(LabelDecl),
  Macro((TokenTree, SimpleSpan), (TokenTree, SimpleSpan)),
  Statement(StatementDecl),
}
impl core::fmt::Debug for BlockElement {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      BlockElement::Label(l) => core::fmt::Debug::fmt(&l, f),
      BlockElement::Statement(s) => core::fmt::Debug::fmt(&s, f),
      BlockElement::Macro((name, _), (args, _)) => write!(f, "{name:?}!{args:?};"),
    }
  }
}
impl BlockElement {
  pub fn parser<'a, I>() -> impl Parser<'a, I, Self, ErrRichTokenTree<'a>>
  where
    I: ValueInput<'a, Token = TokenTree, Span = SimpleSpan>,
  {
    let label = LabelDecl::parser().map(BlockElement::Label);
    let statement = StatementDecl::parser().map(BlockElement::Statement);
    let macro_use = {
      let name = select! {
        Lone(Ident(i)) => Lone(Ident(i)),
      };
      let bang = just(Lone(Punct('!')));
      let args = select! {
        Parens(tt) => Parens(tt),
      };
      let semicolon = just(Lone(Punct(';')));
      name
        .map_with_span(id2)
        .then_ignore(bang)
        .then(args.map_with_span(id2))
        .then_ignore(semicolon)
        .map(|(name, args)| Self::Macro(name, args))
    };

    label.or(macro_use).or(statement)
  }
}
