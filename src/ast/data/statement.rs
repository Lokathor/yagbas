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
impl Statement {
  pub fn map_expressions<F>(&mut self, op: &mut F)
  where
    F: FnMut(FileSpanned<Expression>) -> Expression,
  {
    match self {
      Statement::Expression(x) => {
        let exp: FileSpanned<Expression> = FileSpanned::take(x);
        **x = op(exp);
      }
      Statement::IfElse(if_else) => {
        let exp: FileSpanned<Expression> = FileSpanned::take(&mut if_else.test);
        if_else.test._payload = op(exp);
      }
      Statement::Loop(file_spanned) => todo!(),
      Statement::Break(_)
      | Statement::Continue(_)
      | Statement::Call(_)
      | Statement::Return
      | Statement::StatementError => {
        todo!()
      }
    }
  }
}
