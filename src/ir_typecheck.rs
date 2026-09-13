use slotmap::SlotMap;

use crate::{
  ast::Ast,
  ir_nameres::{TypeNameId, TypeNameInfo, VarNameId, VarNameInfo},
};

#[derive(Debug, Clone, Default)]
pub struct IrTypecheck {
  pub ast: Ast,
  pub var_names: SlotMap<VarNameId, VarNameInfo>,
  pub type_names: SlotMap<TypeNameId, TypeNameInfo>,
  // TODO: what the heck other info do we store?
}
