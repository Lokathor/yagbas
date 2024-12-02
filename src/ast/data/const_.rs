use super::*;

#[derive(Debug, Clone)]
pub struct Const {
  pub name: FileSpanned<StrID>,
  pub expression: FileSpanned<Expression>,
}
