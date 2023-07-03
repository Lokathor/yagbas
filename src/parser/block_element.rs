use super::*;

#[derive(Clone)]
pub enum BlockElement {
  Label(LabelDecl),
  Statement(StatementDecl),
}
impl core::fmt::Debug for BlockElement {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      BlockElement::Label(l) => core::fmt::Debug::fmt(&l, f),
      BlockElement::Statement(s) => core::fmt::Debug::fmt(&s, f),
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

    label.or(statement)
  }
}
