use crate::{src_files::FileSpanned, str_id::StrID, token_tree::TokenTree};
use core::sync::atomic::{AtomicUsize, Ordering};

/// Generates an ID value for a `Statement::Loop`.
pub fn get_next_loop_id() -> usize {
  static NEXT_LOOP_ID: AtomicUsize = AtomicUsize::new(1);
  NEXT_LOOP_ID.fetch_add(1, Ordering::Relaxed)
}

#[derive(Debug, Clone)]
pub enum Statement {
  Return,
  Loop { name: StrID, id: usize, statements: Vec<FileSpanned<Statement>> },
  Call { target: StrID, args: Vec<FileSpanned<TokenTree>> },
  StatementError,
}
impl Statement {
  #[inline]
  pub fn new_loop(
    name: StrID, statements: Vec<FileSpanned<Statement>>,
  ) -> Self {
    Self::Loop { name, statements, id: get_next_loop_id() }
  }
}
