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

  pub fn expressions_mut(
    &mut self,
  ) -> impl '_ + InternalIterator<Item = &'_ mut FileSpanned<Expression>> {
    return ExpressionsMut(self);
    // where:
    struct ExpressionsMut<'r>(&'r mut Statement);
    impl<'r> ExpressionsMut<'r> {
      fn try_for_each_helper<R, F>(self, mut f: &mut F) -> ControlFlow<R>
      where
        F: FnMut(&'r mut FileSpanned<Expression>) -> ControlFlow<R>,
      {
        match self.0 {
          Statement::Expression(xpr) => f(xpr)?,
          Statement::IfElse(if_else) => {
            if_else.expressions_mut().try_for_each(f)?
          }
          Statement::Loop(loop_) => loop_.expressions_mut().try_for_each(f)?,
          Statement::Break(_)
          | Statement::Continue(_)
          | Statement::Call(_)
          | Statement::Return
          | Statement::StatementError => {}
        }
        ControlFlow::Continue(())
      }
    }

    impl<'r> InternalIterator for ExpressionsMut<'r> {
      type Item = &'r mut FileSpanned<Expression>;

      fn try_for_each<R, F>(self, mut f: F) -> ControlFlow<R>
      where
        F: FnMut(Self::Item) -> ControlFlow<R>,
      {
        self.try_for_each_helper(&mut f)
      }
    }
  }
}
