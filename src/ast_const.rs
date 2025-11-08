use super::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AstConst {
  pub name: StrID,
  pub name_span: Span32,
  pub ty: StrID,
  pub ty_span: Span32,
  pub expr: Expr,
  pub expr_span: Span32,
  pub attributes: Vec<AstAttribute>,
  pub file_id: FileID,
  pub total_span: Span32,
}
