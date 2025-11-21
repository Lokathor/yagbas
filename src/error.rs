use super::*;
use chumsky::error::{RichPattern, RichReason};
use core::iter::IntoIterator;
use std::sync::{Mutex, PoisonError};

#[derive(Debug, Clone)]
pub enum YagError {
  IO(PathBuf, String),
  TokenTreeParseError(FileID, Rich<'static, Token, Span32>),
}

pub static ERROR_BUCKET: Mutex<Vec<YagError>> = Mutex::new(Vec::new());

/// Calls `log_error_iter` for a single error.
pub fn log_error(e: YagError) {
  log_error_iter([e]);
}

pub fn log_error_iter<I: IntoIterator<Item = YagError>>(i: I) {
  let mut locked_vec =
    ERROR_BUCKET.lock().unwrap_or_else(PoisonError::into_inner);
  locked_vec.extend(i);
}

/// Returns `true` if there is an error printed.
pub fn print_any_errors() -> bool {
  let mut locked_vec =
    ERROR_BUCKET.lock().unwrap_or_else(PoisonError::into_inner);
  // TODO: sort the vec as much as possible before printing.
  for e in locked_vec.iter() {
    eprintln!("{e:?}");
  }
  !locked_vec.is_empty()
}
