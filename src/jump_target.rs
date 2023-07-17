use crate::const_expr::ConstExpr;

use super::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum JumpDir {
  Forward,
  Back,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum JumpTarget {
  Ident(StaticStr),
  Number { num_res: Result<i32, CowStr>, dir: JumpDir },
  ConstExpr(ConstExpr),
}
impl JumpTarget {
  pub fn parser<'a, I>() -> impl Parser<'a, I, Self, ErrRichTokenTree<'a>>
  where
    I: ValueInput<'a, Token = TokenTree, Span = SimpleSpan> + BorrowInput<'a>,
  {
    let ident = ident().map(Self::Ident);

    let number = num_lit().map(|n| {
      if let Some(lit) = n.strip_suffix('b') {
        Self::Number { num_res: lit_to_value(lit), dir: JumpDir::Back }
      } else if let Some(lit) = n.strip_suffix('f') {
        Self::Number { num_res: lit_to_value(lit), dir: JumpDir::Forward }
      } else {
        Self::Number {
          num_res: Err(Cow::Borrowed("Numbered label must end with `b` or `f`")),
          dir: JumpDir::Back,
        }
      }
    });

    let const_expr = ConstExpr::parser().map(Self::ConstExpr);

    choice((ident, number, const_expr))
  }
}
