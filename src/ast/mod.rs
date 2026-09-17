//! Module for the Abstract Syntax Tree types.

use str_id::StrId;

#[derive(Debug, Clone, Default)]
pub struct Ast {
  pub modules: Vec<AstModule>,
  pub errors: Vec<AstError>,
}

#[derive(Debug, Clone)]
pub enum AstError {
  ErrGeneric(String),
}

#[derive(Debug, Clone, Default)]
pub struct AstModule {
  pub file_origin: StrId,
  pub items: Vec<AstItem>,
}

#[derive(Debug, Clone, Default)]
pub struct AstItem {
  //
}
