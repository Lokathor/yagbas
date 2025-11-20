use super::*;

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub enum Statement {
  #[default]
  StatementError,
  
  Expr(Expr),
  Assign(Expr, Expr),
  Let(Expr, Expr),
}
