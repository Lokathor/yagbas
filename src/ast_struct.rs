use super::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AstStruct {
  pub span: Span32,
  pub name: StrID,
  pub name_span: Span32,
  pub attributes: Vec<AstAttribute>,
  pub fields: Vec<AstStructFieldDef>,
  pub file_id: FileID,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AstStructFieldDef {
  pub span: Span32,
  pub name: StrID,
  pub name_span: Span32,
  pub ty: StrID,
  pub ty_span: Span32,
  pub attributes: Vec<AstAttribute>,
}
