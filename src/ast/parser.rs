use crate::{Span, ast::AstError, path_id::PathId};

#[derive(Debug, Clone)]
pub struct AstParser {
  pub file_origin: PathId,
  pub errors: Vec<AstError>,
}
impl AstParser {
  pub fn error_at(&mut self, span: Span, message: String) {
    self.errors.push(AstError { file_origin: self.file_origin, span, message });
  }
}
