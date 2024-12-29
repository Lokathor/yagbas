use super::*;

#[derive(Debug, Clone)]
pub struct Loop {
  pub name: FileSpanned<Option<FileSpanned<StrID>>>,
  pub id: usize,
  pub statements: Vec<FileSpanned<Statement>>,
  pub canonical_name: Option<StrID>,
  pub canonical_start: Option<StrID>,
  pub canonical_end: Option<StrID>,
}
impl Loop {
  /// Gets the next ID value for a new loop.
  ///
  /// This is automatically used by `new` and `new_with_name`.
  #[inline]
  fn generate_next_id() -> usize {
    use core::sync::atomic::{AtomicUsize, Ordering};
    static NEXT_LOOP_ID: AtomicUsize = AtomicUsize::new(1);
    NEXT_LOOP_ID.fetch_add(1, core::sync::atomic::Ordering::Relaxed)
  }

  #[inline]
  #[must_use]
  pub fn new(
    name: FileSpanned<Option<FileSpanned<StrID>>>,
    statements: Vec<FileSpanned<Statement>>,
  ) -> Self {
    Self {
      name,
      statements,
      id: Self::generate_next_id(),
      canonical_name: None,
      canonical_start: None,
      canonical_end: None,
    }
  }

  #[inline]
  pub fn make_canonical_loop_values(&mut self) {
    let x = self.id;
    let name = self.name.as_deref().map(StrID::as_ref).unwrap_or_default();
    self.canonical_name = Some(StrID::from(format!(".loop{x}#{name}")));
    self.canonical_start = Some(StrID::from(format!(".loop{x}#{name}#start")));
    self.canonical_end = Some(StrID::from(format!(".loop{x}#{name}#end")));
    for statement in self.statements.iter_mut() {
      statement.make_canonical_loop_values();
    }
  }

  pub fn expressions_mut(
    &mut self,
  ) -> impl '_ + InternalIteratorMut<ItemMut = &'_ mut FileSpanned<Expression>>
  {
    return ExpressionsMut(self);
    // where:
    struct ExpressionsMut<'r>(&'r mut Loop);
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

  pub fn write_code(
    &self, loop_stack: &mut Vec<(Option<StrID>, StrID)>,
    out: &mut impl Extend<Asm>,
  ) {
    let name = self.name.map(|n| n._payload);
    let target = self.canonical_name.unwrap();
    loop_stack.push((name, target));
    out.extend([Asm::Label(self.canonical_start.unwrap())]);

    for statement in self.statements.iter() {
      statement.write_code(loop_stack, out);
    }
    out.extend([Asm::JumpToLabel(
      Condition::Always,
      self.canonical_start.unwrap(),
    )]);

    out.extend([Asm::Label(self.canonical_end.unwrap())]);
    loop_stack.pop();
  }
}
