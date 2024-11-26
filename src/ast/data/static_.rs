use super::*;

#[derive(Debug, Clone)]
pub struct Static {
  pub name: FileSpanned<StrID>,
  pub ty: FileSpanned<StaticType>,
  pub expr: FileSpanned<StaticExpr>,
}

#[derive(Debug, Clone)]
pub enum StaticType {
  /// An array of the given length
  Array(FileSpanned<ConstExpr>),
  /// An array with a length inferred from the static's expression.
  ArrayInferred,
}

#[derive(Debug, Clone)]
pub enum StaticExpr {
  Array(Vec<FileSpanned<ConstExpr>>),
}
