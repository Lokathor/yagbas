//! "Grant me the power of name resolution!"

use crate::ast::{Ast, PointerAccessKind};
use crate::{ItemId, TypeId};

pub mod name_res;
pub mod type_check;

#[derive(Debug, Clone)]
pub struct IrNameResTypeCheck {
  pub ast: Ast,
}
impl IrNameResTypeCheck {
  pub fn new(ast: Ast) -> Self {
    let mut out = Self { ast };

    out
  }
}

/// The types that a local variable can be.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
  ErrType,
  Never,
  LiteralInteger,
  Unit,
  Bool,
  U8,
  I8,
  U16,
  I16,
  Array { elem_id: TypeId, elem_count: u32 },
  Pointer { target_id: TypeId, access_kind: PointerAccessKind },
  Function { arg_ids: Vec<TypeId>, ret: TypeId },
  Struct(ItemId),
  Bitbag(ItemId),
  Enum(ItemId),
}
