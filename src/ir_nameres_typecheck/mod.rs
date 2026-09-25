//! "Grant me the power of name resolution!"

use crate::ast::{Ast, PointerAccessKind, TypeExprKind};
use crate::{FnvBiHashMap, ItemId, TypeId, ValueExprId};

use fnv::FnvHashMap;

pub mod name_res;
pub mod type_check;

#[derive(Debug, Clone)]
pub struct IrNameResTypeCheck {
  pub ast: Ast,
  pub type_database: FnvBiHashMap<TypeId, Type>,
  pub expr_types: FnvHashMap<ValueExprId, TypeId>,
}
impl IrNameResTypeCheck {
  pub fn new(ast: Ast) -> Self {
    let mut out = Self {
      ast,
      expr_types: Default::default(),
      type_database: Default::default(),
    };

    out
  }

  pub fn type_id_from_type(&mut self, ty: Type) -> TypeId {
    match self.type_database.get_by_right(&ty) {
      Some(id) => *id,
      None => {
        let id = TypeId::new();
        self.type_database.insert(id, ty);
        id
      }
    }
  }

  pub fn type_from_type_expr_kind(
    &mut self, tyx_kind: &TypeExprKind,
  ) -> Result<Type, ()> {
    match tyx_kind {
      TypeExprKind::Identifier(_) => Err(()),
      TypeExprKind::ErrTypeExprKind => Ok(Type::ErrType),
      TypeExprKind::Array { elem_tyx, elem_count: _ } => {
        let elem_ty = self.type_from_type_expr_kind(&elem_tyx.kind)?;
        let elem_id = self.type_id_from_type(elem_ty);
        let elem_count = Err(())?; // TODO: resolve element counts properly.
        Ok(Type::Array { elem_id, elem_count })
      }
      TypeExprKind::Pointer { target_tyx: elem_tyx, access_kind } => {
        let target_ty = self.type_from_type_expr_kind(&elem_tyx.kind)?;
        let target_id = self.type_id_from_type(target_ty);
        Ok(Type::Pointer { target_id, access_kind: *access_kind })
      }
      TypeExprKind::NameOfStruct(item_id) => Ok(Type::Struct(*item_id)),
      TypeExprKind::NameOfBitbag(item_id) => Ok(Type::Bitbag(*item_id)),
      TypeExprKind::NameOfEnum(item_id) => Ok(Type::Enum(*item_id)),
      TypeExprKind::Unit => Ok(Type::Unit),
      TypeExprKind::Bool => Ok(Type::Bool),
      TypeExprKind::U8 => Ok(Type::U8),
      TypeExprKind::I8 => Ok(Type::I8),
      TypeExprKind::U16 => Ok(Type::U16),
      TypeExprKind::I16 => Ok(Type::I16),
    }
  }

  pub fn type_id_from_type_expr_kind(
    &mut self, tyx_kind: &TypeExprKind,
  ) -> Result<TypeId, ()> {
    let ty = self.type_from_type_expr_kind(tyx_kind)?;
    Ok(self.type_id_from_type(ty))
  }
}

/// The types that a local variable can be.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
  ErrType,
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
