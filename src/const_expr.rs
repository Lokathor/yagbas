use crate::macro_use::MacroUse;

use super::*;

#[derive(Clone, PartialEq, Eq)]
pub enum ConstExpr {
  Ident(StaticStr),
  Lit(Result<i32, CowStr>),
  MacroUse(MacroUse),
}
impl ConstExpr {
  pub fn parser<'a, I>() -> impl Parser<'a, I, Self, ErrRichTokenTree<'a>>
  where
    I: ValueInput<'a, Token = TokenTree, Span = SimpleSpan>,
  {
    let ident = ident().map(Self::Ident);

    let lit = num_lit().validate(|lit, _span, _emitter| lit_to_value(lit)).map(Self::Lit);

    let macro_use = MacroUse::parser().map(Self::MacroUse);

    choice((macro_use, ident, lit))
  }
}
impl core::fmt::Debug for ConstExpr {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      ConstExpr::Ident(x) => write!(f, "{x}"),
      ConstExpr::Lit(x) => write!(f, "{x:?}"),
      ConstExpr::MacroUse(x) => write!(f, "{x:?}"),
    }
  }
}
