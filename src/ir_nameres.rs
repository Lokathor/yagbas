use slotmap::{SlotMap, new_key_type};
use str_id::StrId;

use crate::ast::Ast;

new_key_type! { pub struct NameId; }

/// Intermediate Representation with Name Resolution
#[derive(Debug, Clone, Default)]
pub struct IrNr {
  pub ast: Ast,
  pub names: SlotMap<NameId, StrId>,
}
