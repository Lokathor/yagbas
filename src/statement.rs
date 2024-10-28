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
  Loop(Loop),
  Call { target: StrID, args: Vec<FileSpanned<TokenTree>> },
  StatementError,
}

#[derive(Debug, Clone)]
pub struct Loop {
  pub name: StrID,
  pub id: usize,
  pub statements: Vec<FileSpanned<Statement>>,
}
impl Loop {
  #[inline]
  pub fn new(statements: Vec<FileSpanned<Statement>>) -> Self {
    Self::new_with_name(StrID::from(""), statements)
  }
  #[inline]
  pub fn new_with_name(
    name: StrID, statements: Vec<FileSpanned<Statement>>,
  ) -> Self {
    Self { name, statements, id: get_next_loop_id() }
  }
}
