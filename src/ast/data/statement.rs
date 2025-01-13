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
  ) -> impl '_ + InternalIteratorRec<Item = &'_ mut FileSpanned<Expression>> {
    adhoc_internal_iterator_rec!(
      'r, self, |this: &'r mut Statement, yield_| -> &'r mut FileSpanned<Expression> {
        match this {
          Statement::Expression(xpr) => yield_(xpr)?,
          Statement::IfElse(if_else) => {
            if_else.expressions_mut().try_for_each_rec(yield_)?
          }
          Statement::Loop(loop_) => {
            loop_.expressions_mut().try_for_each_rec(yield_)?
          }
          Statement::Break(_)
          | Statement::Continue(_)
          | Statement::Call(_)
          | Statement::Return
          | Statement::StatementError => {}
        }
      }
    )
  }

  pub fn calls_ref(
    &self,
  ) -> impl '_ + InternalIteratorRec<Item = &'_ FileSpanned<Call>> {
    adhoc_internal_iterator_rec!(
      'r, self, |this: &'r Statement, yield_| -> &'r FileSpanned<Call> {
        match this {
          Statement::Call(c) => yield_(c)?,
          Statement::IfElse(if_else) => {
            if_else.calls_ref().try_for_each_rec(yield_)?
          }
          Statement::Loop(loop_) => loop_.calls_ref().try_for_each_rec(yield_)?,
          Statement::Expression(_)
          | Statement::Break(_)
          | Statement::Continue(_)
          | Statement::Return
          | Statement::StatementError => {}
        }
      }
    )
  }

  pub fn calls_mut(
    &mut self,
  ) -> impl '_ + InternalIteratorRec<Item = &'_ mut FileSpanned<Call>> {
    adhoc_internal_iterator_rec!(
      'r, self, |this: &'r mut Statement, yield_| -> &'r mut FileSpanned<Call> {
        match this {
          Statement::Call(c) => yield_(c)?,
          Statement::IfElse(if_else) => {
            if_else.calls_mut().try_for_each_rec(yield_)?
          }
          Statement::Loop(loop_) => loop_.calls_mut().try_for_each_rec(yield_)?,
          Statement::Expression(_)
          | Statement::Break(_)
          | Statement::Continue(_)
          | Statement::Return
          | Statement::StatementError => {}
        }
      }
    )
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
