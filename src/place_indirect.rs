use crate::*;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum PlaceIndirect {
  AddrBC,
  AddrDE,
  AddrHLInc,
  AddrHLDec,
}
impl PlaceIndirect {
  pub fn parser<'a, I>() -> impl Parser<'a, I, Self, ErrRichTokenTree<'a>>
  where
    I: ValueInput<'a, Token = TokenTree, Span = SimpleSpan>,
  {
    select! {
      Lone(Token::AddrBC) => Self::AddrBC,
      Lone(Token::AddrDE) => Self::AddrDE,
      Lone(Token::AddrHLInc) => Self::AddrHLInc,
      Lone(Token::AddrHLDec) => Self::AddrHLDec,
    }
  }
}
impl core::fmt::Debug for PlaceIndirect {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{}",
      match self {
        Self::AddrBC => "[bc]",
        Self::AddrDE => "[de]",
        Self::AddrHLInc => "[hl++]",
        Self::AddrHLDec => "[hl--]",
      }
    )
  }
}
