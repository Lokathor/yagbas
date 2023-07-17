use crate::macro_use::MacroUse;

use super::*;

#[derive(Clone, PartialEq, Eq)]
pub enum ConstExpr {
  /// A successfully evaluated value.
  Value(i32),
  /// Names some other const we should get the value of.
  Ident(StaticStr),
  /// A macro we should evaluate.
  MacroUse(MacroUse),
  /// The user wrote a literal but we couldn't parse it (and why).
  BadLit(CowStr),
  /// zesterer please make this all make sense some day
  ConstExprError,
}
impl ConstExpr {
  pub fn parser<'a, I>() -> impl Parser<'a, I, Self, ErrRichTokenTree<'a>> + Clone
  where
    I: ValueInput<'a, Token = TokenTree, Span = SimpleSpan> + BorrowInput<'a>,
  {
    let lit = num_lit().map(|lit| match lit_to_value(lit) {
      Ok(i) => Self::Value(i),
      Err(e) => Self::BadLit(e),
    });

    let macro_use = MacroUse::parser().map(Self::MacroUse);

    let ident = ident().map(Self::Ident);

    choice((lit, macro_use, ident))
  }
}
impl core::fmt::Debug for ConstExpr {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      ConstExpr::Ident(x) => write!(f, "{x}"),
      ConstExpr::Value(x) => write!(f, "{x}"),
      ConstExpr::BadLit(x) => write!(f, "{x:?}"),
      ConstExpr::MacroUse(x) => write!(f, "{x:?}"),
      ConstExpr::ConstExprError => write!(f, "/*ConstExprError*/"),
    }
  }
}
