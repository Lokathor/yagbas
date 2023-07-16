use crate::{const_decl::ConstDecl, const_expr::ConstExpr};

use super::*;

#[derive(Clone, PartialEq, Eq)]
pub struct ConstFormed {
  pub name: (StaticStr, SimpleSpan),
  pub expr: (ConstExpr, SimpleSpan),
}
impl TryFrom<ConstDecl> for ConstFormed {
  type Error = Vec<CowStr>;
  fn try_from(decl: ConstDecl) -> Result<Self, Self::Error> {
    Ok(Self {
      name: decl.name,
      expr: run_parser(ConstExpr::parser().map_with_span(id2), &decl.expr)
        .into_result()
        .map_err(|errs| {
          errs.into_iter().map(|e| Cow::Owned(format!("{e:?}"))).collect::<Vec<_>>()
        })?,
    })
  }
}
impl core::fmt::Debug for ConstFormed {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "ConstFormed {:?} = {:?};", self.name.0, self.expr.0)
  }
}
