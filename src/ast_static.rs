use super::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AstStatic {
  pub name: StrID,
  pub name_span: Span32,
  pub ty: StrID,
  pub ty_span: Span32,
  pub expr: Expr,
  pub expr_span: Span32,
  pub attributes: Vec<AstAttribute>,
  pub file_id: FileID,
  pub total_span: Span32,
  pub mutability: StaticMutability,
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub enum StaticMutability {
  #[default]
  Immutable,
  Mutable,
  MemoryMappedIO,
}
