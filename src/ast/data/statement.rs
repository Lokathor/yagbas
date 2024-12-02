use super::*;

#[derive(Debug, Clone)]
pub enum Statement {
  Expression(FileSpanned<Expression>),
  IfElse(FileSpanned<IfElse>),
  Loop(FileSpanned<Loop>),
  Break(FileSpanned<Option<FileSpanned<StrID>>>),
  Continue(FileSpanned<Option<FileSpanned<StrID>>>),
  Call(FileSpanned<Call>),
  Return,
  StatementError,
}
