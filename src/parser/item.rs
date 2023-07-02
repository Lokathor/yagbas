use super::*;

#[derive(Debug, Clone)]
pub enum Item {
  ConstDecl(ConstDecl),
  Section(Section),
}
impl Item {
  pub fn parser<'a, I>() -> impl Parser<'a, I, Self, ErrRichTokenTree<'a>>
  where
    I: ValueInput<'a, Token = TokenTree, Span = SimpleSpan>,
  {
    let const_decl = ConstDecl::parser().map(Item::ConstDecl);
    let section = Section::parser().map(Item::Section);

    const_decl.or(section)
  }
}
