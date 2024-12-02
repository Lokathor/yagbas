use super::*;

#[derive(Debug, Clone)]
pub struct Static {
  pub name: FileSpanned<StrID>,
  pub bytes: Vec<FileSpanned<Expression>>,
}
