use super::*;

#[derive(Debug, Clone)]
pub struct Loop {
  pub name: FileSpanned<Option<FileSpanned<StrID>>>,
  pub id: usize,
  pub statements: Vec<FileSpanned<Statement>>,
  pub canonical_name: StrID,
  pub canonical_start: StrID,
  pub canonical_end: StrID,
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
    let name_str = name.map(|id| id.as_str()).unwrap_or_default();
    let id = Self::generate_next_id();
    Self {
      name,
      statements,
      id,
      canonical_name: StrID::from(format!(".loop{id}#{name_str}")),
      canonical_start: StrID::from(format!(".loop{id}#{name_str}#start")),
      canonical_end: StrID::from(format!(".loop{id}#{name_str}#end")),
    }
  }

  pub fn expressions_mut(
    &mut self,
  ) -> impl '_ + InternalIteratorRec<ItemRec = &'_ mut FileSpanned<Expression>>
  {
    return ExpressionsMut(self);
    // where:
    struct ExpressionsMut<'r>(&'r mut Loop);
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

  pub fn calls_ref(
    &self,
  ) -> impl '_ + InternalIteratorRec<ItemRec = &'_ FileSpanned<Call>> {
    return CallsRef(self);
    // where:
    struct CallsRef<'r>(&'r Loop);
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
    struct CallsMut<'r>(&'r mut Loop);
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

  pub fn write_code(
    &self, loop_stack: &mut Vec<(Option<StrID>, StrID)>,
    out: &mut impl Extend<Asm>,
  ) {
    let name = self.name.map(|n| n._payload);
    let target = self.canonical_name;
    loop_stack.push((name, target));
    out.extend([Asm::Label(self.canonical_start)]);

    for statement in self.statements.iter() {
      statement.write_code(loop_stack, out);
    }
    out.extend([Asm::JumpToLabel(Condition::Always, self.canonical_start)]);

    out.extend([Asm::Label(self.canonical_end)]);
    loop_stack.pop();
  }
}
