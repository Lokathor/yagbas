use super::*;

#[derive(Debug, Clone)]
pub enum Statement {
  Expression(FileSpanned<Expression>),
  StatementError,
}
