use super::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AstFunction {
  span: Span32,
  file_id: FileID,
  name: StrID,
  name_span: Span32,
  args: Vec<AstFunctionArg>,
  return_ty: StrID,
  return_ty_span: Span32,
  body: Vec<Statement>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AstFunctionArg {
  name: StrID,
  name_span: Span32,
  ty: StrID,
  ty_span: Span32,
}
