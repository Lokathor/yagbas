use super::*;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Cond {
  CY,
  NC,
  ZE,
  NZ,
  AL,
}
impl Cond {
  pub fn parser<'a, I>() -> impl Parser<'a, I, Self, ErrRichTokenTree<'a>>
  where
    I: ValueInput<'a, Token = TokenTree, Span = SimpleSpan>,
  {
    select! {
      Lone(Token::CondCY) => Self::CY,
      Lone(Token::CondNC) => Self::NC,
      Lone(Token::CondZE) => Self::ZE,
      Lone(Token::CondNZ) => Self::NZ,
      Lone(Token::CondAL) => Self::AL,
    }
  }
}
impl core::fmt::Debug for Cond {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Cond::CY => write!(f, "cy"),
      Cond::NC => write!(f, "nc"),
      Cond::ZE => write!(f, "ze"),
      Cond::NZ => write!(f, "nz"),
      Cond::AL => write!(f, "al"),
    }
  }
}
