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
  ) -> impl '_ + InternalIteratorRec<ItemRec = &'_ mut FileSpanned<Expression>>
  {
    return ExpressionsMut(self);
    // where:
    struct ExpressionsMut<'r>(&'r mut Function);
    impl<'r> InternalIterator for ExpressionsMut<'r> {
      internal_iterator_rec_guts! {}
    }

    impl<'r> InternalIteratorRec for ExpressionsMut<'r> {
      type ItemRec = &'r mut FileSpanned<Expression>;

      fn try_for_each_rec<R, F>(self, f: &mut F) -> ControlFlow<R>
      where
        F: FnMut(Self::Item) -> ControlFlow<R>,
      {
        for stmt in self.0.statements.iter_mut() {
          stmt.expressions_mut().try_for_each_rec(&mut *f)?;
        }
        ControlFlow::Continue(())
      }
    }
  }

  //#[cfg(target_os = "none")]
  pub fn calls_ref(
    &self,
  ) -> impl '_ + InternalIteratorRec<Item = &'_ FileSpanned<Call>> {
    return CallsRef(self);
    // where:
    struct CallsRef<'r>(&'r Function);
    impl<'r> InternalIterator for CallsRef<'r> {
      internal_iterator_rec_guts! {}
    }

    impl<'r> InternalIteratorRec for CallsRef<'r> {
      type ItemRec = &'r FileSpanned<Call>;

      fn try_for_each_rec<R, F>(self, f: &mut F) -> ControlFlow<R>
      where
        F: FnMut(Self::Item) -> ControlFlow<R>,
      {
        for stmt in self.0.statements.iter() {
          stmt.calls_ref().try_for_each_rec(&mut *f)?;
        }
        ControlFlow::Continue(())
      }
    }
  }

  pub fn calls_mut(
    &mut self,
  ) -> impl '_ + InternalIteratorRec<ItemRec = &'_ mut FileSpanned<Call>> {
    return CallsMut(self);
    // where:
    struct CallsMut<'r>(&'r mut Function);
    impl<'r> InternalIterator for CallsMut<'r> {
      internal_iterator_rec_guts! {}
    }

    impl<'r> InternalIteratorRec for CallsMut<'r> {
      type ItemRec = &'r mut FileSpanned<Call>;

      fn try_for_each_rec<R, F>(self, f: &mut F) -> ControlFlow<R>
      where
        F: FnMut(Self::Item) -> ControlFlow<R>,
      {
        for stmt in self.0.statements.iter_mut() {
          stmt.calls_mut().try_for_each_rec(&mut *f)?;
        }
        ControlFlow::Continue(())
      }
    }
  }

  pub fn generate_code(&self) -> Vec<Asm> {
    let mut out = Vec::new();
    let mut loop_stack = Vec::new();

    let label = Asm::Label(self.name._payload);
    out.push(label);
    for statement in self.statements.iter() {
      statement.write_code(&mut loop_stack, &mut out);
    }

    out
  }
}
