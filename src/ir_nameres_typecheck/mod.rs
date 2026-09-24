//! "Grant me the power of name resolution!"

use crate::ast::{Ast, PointerAccessKind};
use crate::{ItemId, TypeId, ValueExprId};

use bimap::BiHashMap;
use fnv::FnvHashMap;

pub type BiMapType =
  BiHashMap<TypeId, Type, fnv::FnvBuildHasher, fnv::FnvBuildHasher>;

pub mod name_res;
pub mod type_check;

#[derive(Debug, Clone)]
pub struct IrNameResTypeCheck {
  pub ast: Ast,
  pub type_database: BiMapType,
  pub expr_types: FnvHashMap<ValueExprId, TypeId>,
}

/// The types that a local variable can be.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
  ErrType,
  MagicIntegerLiteral,
  SimplePrimitive(&'static str),
  Array { elem_ty: TypeId, elem_count: u32 },
  Pointer { target_ty: TypeId, access_kind: PointerAccessKind },
  Function { args: Vec<TypeId>, ret: TypeId },
  Struct(ItemId),
  Bitbag(ItemId),
  Enum(ItemId),
}
pub static PRIMITIVE_TYPE_NAMES: &[&str] =
  &["()", "bool", "u8", "i8", "u16", "i16"];
