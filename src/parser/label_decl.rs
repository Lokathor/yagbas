use super::*;

#[derive(Clone, Copy)]
pub enum LabelDecl {
  Ident(StaticStr),
  Num(StaticStr),
}
impl core::fmt::Debug for LabelDecl {
  /// this cuts out some of the SimpleSpan junk from a debug print compared to
  /// using the derive.
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      LabelDecl::Ident(i) => write!(f, "{i}:"),
      LabelDecl::Num(n) => write!(f, "{n}:"),
    }
  }
}
impl LabelDecl {
  pub fn parser<'a, I>() -> impl Parser<'a, I, Self, ErrRichTokenTree<'a>>
  where
    I: ValueInput<'a, Token = TokenTree, Span = SimpleSpan>,
  {
    let name = select! {
      Lone(Ident(i)) => LabelDecl::Ident(i),
      Lone(NumLit(i)) => LabelDecl::Num(i),
    };

    name.then_ignore(just(Lone(Punct(':'))))
  }
}
