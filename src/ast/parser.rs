use std::path::PathBuf;

use crate::{Span, ast::AstError};

#[derive(Debug, Clone)]
pub struct AstParser {
  pub file_origin: PathBuf,
  pub errors: Vec<AstError>,
}
impl AstParser {
  pub fn error_at(&mut self, span: Span, message: String) {
    self.errors.push(AstError {
      file_origin: self.file_origin.clone(),
      span,
      message,
    });
  }
}
