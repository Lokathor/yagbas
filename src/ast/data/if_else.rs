use super::*;

#[derive(Debug, Clone)]
pub struct IfElse {
  pub test: FileSpanned<Expression>,
  pub if_body: Vec<FileSpanned<Statement>>,
  pub else_body: Vec<FileSpanned<Statement>>,
}
impl IfElse {
  pub fn expressions_mut(
    &mut self,
  ) -> impl '_ + InternalIteratorMut<ItemMut = &'_ mut FileSpanned<Expression>>
  {
    return ExpressionsMut(self);
    // where:
    struct ExpressionsMut<'r>(&'r mut IfElse);
    impl<'r> InternalIterator for ExpressionsMut<'r> {
      internal_iterator_guts! {}
    }

    impl<'r> InternalIteratorMut for ExpressionsMut<'r> {
      type ItemMut = &'r mut FileSpanned<Expression>;

      fn try_for_each_mut<R, F>(self, f: &mut F) -> ControlFlow<R>
      where
        F: FnMut(Self::Item) -> ControlFlow<R>,
      {
        f(&mut self.0.test)?;
        for stmt in self.0.if_body.iter_mut() {
          stmt.expressions_mut().try_for_each_mut(&mut *f)?;
        }
        for stmt in self.0.else_body.iter_mut() {
          stmt.expressions_mut().try_for_each_mut(&mut *f)?;
        }
        ControlFlow::Continue(())
      }
    }
  }
}
