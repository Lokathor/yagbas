use crate::{Span, YagError, path_id::PathId};

#[derive(Debug, Clone)]
pub struct AstParser {
  pub file_origin: PathId,
  pub errors: Vec<YagError>,
}
impl AstParser {
  pub fn error_at(&mut self, span: Span, message: String) {
    self.errors.push(YagError { file_origin: self.file_origin, span, message });
  }
}
