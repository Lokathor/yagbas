use super::*;

#[derive(Debug, Clone)]
pub struct IfElse {
  pub test: FileSpanned<Expression>,
  pub if_body: Vec<FileSpanned<Statement>>,
  pub else_body: Option<Vec<FileSpanned<Statement>>>,
}
