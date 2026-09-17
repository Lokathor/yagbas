use str_id::StrId;

use crate::ast::AstError;

#[derive(Debug, Clone)]
pub struct AstParser {
  pub file_origin: StrId,
  pub errors: Vec<AstError>,
}
