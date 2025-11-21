use super::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AstBitstruct {
  pub span: Span32,
  pub name: StrID,
  pub name_span: Span32,
  pub attributes: Vec<AstAttribute>,
  pub fields: Vec<AstBitstructFieldDef>,
  pub file_id: FileID,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AstBitstructFieldDef {
  pub span: Span32,
  pub name: StrID,
  pub name_span: Span32,
  pub bit: Expr,
  pub bit_span: Span32,
  pub attributes: Vec<AstAttribute>,
}
