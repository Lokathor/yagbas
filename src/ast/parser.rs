use std::path::PathBuf;

use crate::ast::AstError;

#[derive(Debug, Clone)]
pub struct AstParser {
  pub file_origin: PathBuf,
  pub errors: Vec<AstError>,
}
