use super::*;

#[derive(Debug, Clone)]
pub struct Function {
  pub name: FileSpanned<StrID>,
  pub args: Vec<FileSpanned<TokenTree>>,
  pub statements: Vec<FileSpanned<Statement>>,
}
impl Function {
  pub fn expressions_mut(
    &mut self,
  ) -> impl '_ + InternalIteratorMut<ItemMut = &'_ mut FileSpanned<Expression>>
  {
    return ExpressionsMut(self);
    // where:
    struct ExpressionsMut<'r>(&'r mut Function);
    impl<'r> InternalIterator for ExpressionsMut<'r> {
      internal_iterator_guts! {}
    }

    impl<'r> InternalIteratorMut for ExpressionsMut<'r> {
      type ItemMut = &'r mut FileSpanned<Expression>;

      fn try_for_each_mut<R, F>(self, f: &mut F) -> ControlFlow<R>
      where
        F: FnMut(Self::Item) -> ControlFlow<R>,
      {
        for stmt in self.0.statements.iter_mut() {
          stmt.expressions_mut().try_for_each_mut(&mut *f)?;
        }
        ControlFlow::Continue(())
      }
    }
  }

  pub fn generated_code(&self) -> Vec<Asm> {
    let mut out = Vec::new();

    let label =
      Asm::Label(StrID::from(format!("fn#{name}", name = self.name).as_str()));
    out.push(label);

    out
  }
}
