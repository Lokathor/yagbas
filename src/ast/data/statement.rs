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
  pub fn expressions_mut(
    &mut self,
  ) -> impl '_ + InternalIteratorRec<ItemRec = &'_ mut FileSpanned<Expression>>
  {
    return ExpressionsMut(self);
    // where:
    struct ExpressionsMut<'r>(&'r mut Statement);
    impl<'r> InternalIterator for ExpressionsMut<'r> {
      internal_iterator_rec_guts! {}
    }

    impl<'r> InternalIteratorRec for ExpressionsMut<'r> {
      type ItemRec = &'r mut FileSpanned<Expression>;

      fn try_for_each_rec<R, F>(self, f: &mut F) -> ControlFlow<R>
      where
        F: FnMut(Self::Item) -> ControlFlow<R>,
      {
        match self.0 {
          Statement::Expression(xpr) => f(xpr)?,
          Statement::IfElse(if_else) => {
            if_else.expressions_mut().try_for_each_rec(f)?
          }
          Statement::Loop(loop_) => {
            loop_.expressions_mut().try_for_each_rec(f)?
          }
          Statement::Break(_)
          | Statement::Continue(_)
          | Statement::Call(_)
          | Statement::Return
          | Statement::StatementError => {}
        }
        ControlFlow::Continue(())
      }
    }
  }

  pub fn calls_ref(
    &self,
  ) -> impl '_ + InternalIteratorRec<ItemRec = &'_ FileSpanned<Call>> {
    return CallsRef(self);
    // where:
    struct CallsRef<'r>(&'r Statement);
    impl<'r> InternalIterator for CallsRef<'r> {
      internal_iterator_rec_guts! {}
    }

    impl<'r> InternalIteratorRec for CallsRef<'r> {
      type ItemRec = &'r FileSpanned<Call>;

      fn try_for_each_rec<R, F>(self, f: &mut F) -> ControlFlow<R>
      where
        F: FnMut(Self::Item) -> ControlFlow<R>,
      {
        match self.0 {
          Statement::Call(c) => f(c)?,
          Statement::IfElse(if_else) => {
            if_else.calls_ref().try_for_each_rec(f)?
          }
          Statement::Loop(loop_) => loop_.calls_ref().try_for_each_rec(f)?,
          Statement::Expression(_)
          | Statement::Break(_)
          | Statement::Continue(_)
          | Statement::Return
          | Statement::StatementError => {}
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
    struct CallsMut<'r>(&'r mut Statement);
    impl<'r> InternalIterator for CallsMut<'r> {
      internal_iterator_rec_guts! {}
    }

    impl<'r> InternalIteratorRec for CallsMut<'r> {
      type ItemRec = &'r mut FileSpanned<Call>;

      fn try_for_each_rec<R, F>(self, f: &mut F) -> ControlFlow<R>
      where
        F: FnMut(Self::Item) -> ControlFlow<R>,
      {
        match self.0 {
          Statement::Call(c) => f(c)?,
          Statement::IfElse(if_else) => {
            if_else.calls_mut().try_for_each_rec(f)?
          }
          Statement::Loop(loop_) => loop_.calls_mut().try_for_each_rec(f)?,
          Statement::Break(_)
          | Statement::Continue(_)
          | Statement::Expression(_)
          | Statement::Return
          | Statement::StatementError => {}
        }
        ControlFlow::Continue(())
      }
    }
  }

  pub fn write_code(
    &self, loop_stack: &mut Vec<(Option<StrID>, StrID)>,
    out: &mut impl Extend<Asm>,
  ) {
    match self {
      Statement::Expression(xpr) => {
        xpr.write_code(out);
      }
      Statement::IfElse(ifelse) => {
        ifelse.write_code(loop_stack, out);
      }
      Statement::Loop(loop_) => loop_.write_code(loop_stack, out),
      Statement::Break(src_target) => {
        if let Some(canonical) = loop_stack
          .iter()
          .rev()
          .find(|(name, _)| src_target._payload.map(|s| s._payload) == *name)
        {
          let c = canonical.1;
          out.extend([Asm::JumpToLabel(
            Condition::Always,
            StrID::from(format!("{c}#end")),
          )]);
        } else {
          todo!("unhandled missing break target")
        }
      }
      Statement::Continue(src_target) => {
        if let Some(canonical) = loop_stack
          .iter()
          .rev()
          .find(|(name, _)| src_target._payload.map(|s| s._payload) == *name)
        {
          let c = canonical.1;
          out.extend([Asm::JumpToLabel(
            Condition::Always,
            StrID::from(format!("{c}#start")),
          )]);
        } else {
          todo!("unhandled missing continue target")
        }
      }
      // Note(Lokathor): call/return at the top indentation of a function,
      // without an `if` around them, will always happens, so we just fill
      // that in.
      Statement::Call(call) => {
        let Call { target, .. } = call._payload;
        out.extend([Asm::CallLabel(Condition::Always, target._payload)]);
      }
      Statement::Return => out.extend([Asm::Return(Condition::Always)]),
      // Note(Lokathor): We shouldn't be generating code on a function with
      // statement errors within it.
      Statement::StatementError => unimplemented!(),
    }
  }
}
