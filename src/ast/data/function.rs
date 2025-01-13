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
  ) -> impl '_ + InternalIteratorRec<Item = &'_ mut FileSpanned<Expression>> {
    adhoc_internal_iterator_rec!(
      'r, self, |this: &'r mut Function, yield_| -> &'r mut FileSpanned<Expression> {
        for stmt in this.statements.iter_mut() {
          stmt.expressions_mut().try_for_each_rec(yield_)?;
        }
      }
    )
  }

  //#[cfg(target_os = "none")]
  pub fn calls_ref(
    &self,
  ) -> impl '_ + InternalIteratorRec<Item = &'_ FileSpanned<Call>> {
    adhoc_internal_iterator_rec!(
      'r, self, |this: &'r Function, yield_| -> &'r FileSpanned<Call> {
        for stmt in this.statements.iter() {
          stmt.calls_ref().try_for_each_rec(yield_)?;
        }
      }
    )
  }

  pub fn calls_mut(
    &mut self,
  ) -> impl '_ + InternalIteratorRec<Item = &'_ mut FileSpanned<Call>> {
    adhoc_internal_iterator_rec!(
      'r, self, |this: &'r mut Function, yield_| -> &'r mut FileSpanned<Call> {
        for stmt in this.statements.iter_mut() {
          stmt.calls_mut().try_for_each_rec(yield_)?;
        }
      }
    )
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
