//! "Grant me the power of name resolution!"

use crate::ast::{Ast, PointerAccessKind, TypeExprKind};
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

#[allow(unused)]
pub fn type_from_type_expr_kind(
  ir: &mut IrNameResTypeCheck, ty_expr_kind: &TypeExprKind,
) -> Type {
  match ty_expr_kind {
    TypeExprKind::NameOfPrimitive(name) => Type::SimplePrimitive(name),
    TypeExprKind::NameOfStruct(item_id) => todo!(),
    TypeExprKind::NameOfBitbag(item_id) => todo!(),
    TypeExprKind::NameOfEnum(item_id) => todo!(),
    TypeExprKind::Array { elem_ty, elem_count } => todo!(),
    TypeExprKind::Pointer { elem_ty, access_kind } => todo!(),
    TypeExprKind::ErrTypeExprKind => Type::ErrType,
    TypeExprKind::Simple(_) => unimplemented!(),
  }
}
