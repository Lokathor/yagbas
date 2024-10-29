use crate::{src_files::FileSpanned, str_id::StrID, token_tree::TokenTree};
use core::sync::atomic::{AtomicUsize, Ordering};

/// A "single chunk of code" in yagbas.
///
/// Most statements are "one line" of code, but the variants for control flow
/// have a body of inner statements that they cover.
#[derive(Debug, Clone)]
pub enum Statement {
  Return,
  Call { target: StrID, args: Vec<FileSpanned<TokenTree>> },
  Loop(Loop),
  Continue(StrID),
  Break(StrID),
  StatementError,
}

#[derive(Debug, Clone)]
pub struct Loop {
  pub name: StrID,
  pub id: usize,
  pub statements: Vec<FileSpanned<Statement>>,
}
impl Loop {
  /// Gets the next ID value for a new loop.
  ///
  /// This is automatically used by `new` and `new_with_name`.
  #[inline]
  pub fn get_next_id() -> usize {
    static NEXT_LOOP_ID: AtomicUsize = AtomicUsize::new(1);
    NEXT_LOOP_ID.fetch_add(1, Ordering::Relaxed)
  }

  #[inline]
  pub fn new(statements: Vec<FileSpanned<Statement>>) -> Self {
    Self::new_with_name(StrID::default(), statements)
  }
  #[inline]
  pub fn new_with_name(
    name: StrID, statements: Vec<FileSpanned<Statement>>,
  ) -> Self {
    Self { name, statements, id: Self::get_next_id() }
  }

  #[inline]
  pub fn make_start_name(&self) -> String {
    let x = self.id;
    let name = self.name;
    format!(".loop{x}#{name}#start")
  }
  #[inline]
  pub fn make_end_name(&self) -> String {
    let x = self.id;
    let name = self.name;
    format!(".loop{x}#{name}#end")
  }
}
