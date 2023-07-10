use crate::*;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Place8 {
  A,
  B,
  C,
  D,
  E,
  H,
  L,
  AddrHL,
}
impl Place8 {
  pub fn parser<'a, I>() -> impl Parser<'a, I, Self, ErrRichTokenTree<'a>>
  where
    I: ValueInput<'a, Token = TokenTree, Span = SimpleSpan>,
  {
    select! {
      Lone(Token::RegA) => Self::A,
      Lone(Token::RegB) => Self::B,
      Lone(Token::RegC) => Self::C,
      Lone(Token::RegD) => Self::D,
      Lone(Token::RegE) => Self::E,
      Lone(Token::RegH) => Self::H,
      Lone(Token::RegL) => Self::L,
      Lone(Token::AddrHL) => Self::AddrHL,
    }
  }
}
impl core::fmt::Debug for Place8 {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{}",
      match self {
        Self::A => "a",
        Self::B => "b",
        Self::C => "c",
        Self::D => "d",
        Self::E => "e",
        Self::H => "h",
        Self::L => "l",
        Self::AddrHL => "[hl]",
      }
    )
  }
}
