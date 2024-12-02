use super::*;

#[derive(Debug, Clone)]
pub struct Loop {
  pub name: FileSpanned<Option<FileSpanned<StrID>>>,
  pub id: usize,
  pub statements: Vec<FileSpanned<Statement>>,
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
    Self { name, statements, id: Self::generate_next_id() }
  }

  #[inline]
  pub fn make_canonical_start_label(&self) -> String {
    let x = self.id;
    let name = self.name.as_deref().map(StrID::as_ref).unwrap_or_default();
    format!(".loop{x}#{name}#start")
  }
  #[inline]
  pub fn make_canonical_end_label(&self) -> String {
    let x = self.id;
    let name = self.name.as_deref().map(StrID::as_ref).unwrap_or_default();
    format!(".loop{x}#{name}#end")
  }
}
