use crate::*;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Place16 {
  BC,
  DE,
  HL,
  SP,
}
impl Place16 {
  pub fn parser<'a, I>() -> impl Parser<'a, I, Self, ErrRichTokenTree<'a>>
  where
    I: ValueInput<'a, Token = TokenTree, Span = SimpleSpan>,
  {
    select! {
      Lone(Token::RegBC) => Self::BC,
      Lone(Token::RegDE) => Self::DE,
      Lone(Token::RegHL) => Self::HL,
      Lone(Token::RegSP) => Self::SP,
    }
  }
}
impl core::fmt::Debug for Place16 {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{}",
      match self {
        Self::BC => "bc",
        Self::DE => "de",
        Self::HL => "hl",
        Self::SP => "sp",
      }
    )
  }
}
