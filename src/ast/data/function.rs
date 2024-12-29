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

  pub fn generate_code(&self) -> Vec<Asm> {
    let mut out = Vec::new();

    let label =
      Asm::Label(StrID::from(format!("fn#{name}", name = self.name).as_str()));
    out.push(label);
    for statement in self.statements.iter() {
      match &statement._payload {
        Statement::Expression(xpr) => {
          xpr.write_code(&mut out);
        }
        Statement::IfElse(ifelse) => {
          out.push(Asm::Nop);
        }
        Statement::Loop(loop_) => {
          out.push(Asm::Nop);
        }
        // Note(Lokathor): call/return at the top indentation of a function,
        // without an `if` around them, will always happens, so we just fill
        // that in.
        Statement::Call(call) => {
          let Call { target, .. } = call._payload;
          out.push(Asm::CallLabel(Condition::Always, target._payload));
        }
        Statement::Return => out.push(Asm::Return(Condition::Always)),
        // Note(Lokathor): Remember that whatever made the error previously has
        // already put something in the error bucket, and we don't want to
        // double-report problems.
        Statement::StatementError => {
          continue;
        }
        // Note(Lokathor): these should have been cleared out of the statement
        // list by previous stages which check for break/continue having correct
        // targets at all loop levels.
        Statement::Break(_) | Statement::Continue(_) => unimplemented!(),
      }
    }

    out
  }
}
