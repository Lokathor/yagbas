use super::*;
use std::sync::Mutex;
use std::sync::PoisonError;

#[derive(Debug, Clone)]
pub enum YagError {
  IO(PathBuf, String),
  TokenTreeParseError(FileID, Rich<'static, Token>),
  ItemParseError(FileID, Rich<'static, TokenTree>),
  MacroSizeOfStaticNoSize(FileID, SimpleSpan),
  MacroSizeOfStaticBadArgs(FileID, SimpleSpan),
  MacroPalette(FileID, SimpleSpan),
}

pub static ERROR_BUCKET: Mutex<Vec<YagError>> = Mutex::new(Vec::new());

pub fn log_error(e: YagError) {
  let mut locked_vec =
    ERROR_BUCKET.lock().unwrap_or_else(PoisonError::into_inner);
  locked_vec.push(e);
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
