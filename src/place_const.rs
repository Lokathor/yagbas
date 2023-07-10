use crate::*;

#[derive(Clone, PartialEq, Eq)]
pub struct PlaceConst(pub Vec<(TokenTree, SimpleSpan)>);
impl PlaceConst {
  pub fn parser<'a, I>() -> impl Parser<'a, I, Self, ErrRichTokenTree<'a>>
  where
    I: ValueInput<'a, Token = TokenTree, Span = SimpleSpan>,
  {
    select! {
      Brackets(tts) => Self(tts),
    }
  }
}
impl core::fmt::Debug for PlaceConst {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "[")?;
    for (i, x) in self.0.iter().enumerate() {
      if i > 0 {
        write!(f, " ")?;
      }
      write!(f, "{x:?}")?;
    }
    write!(f, "]")?;
    Ok(())
  }
}
