#![allow(unused)]

use crate::ItemId;
use crate::LocalNameId;
use crate::PathId;
use crate::Span;
use crate::YagError;
use crate::ast::Ast;
use crate::ast::PointerAccessKind;
use crate::ast::ValueExpr;
use ena::unify::EqUnifyValue;
use ena::unify::InPlaceUnificationTable;
use ena::unify::UnifyKey;
use imbl::HashMap as ImHashMap;

/// The types that a local variable can be.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
  SimplePrimitive(&'static str),
  Array { elem_ty: Box<Type>, elem_count: u32 },
  Pointer { target_ty: Box<Type>, access_kind: PointerAccessKind },
  Function { args: Vec<Type>, ret: Box<Type> },
  Struct(ItemId),
  Bitbag(ItemId),
  Enum(ItemId),
  //
  Variable(TypeVariable),
}
impl EqUnifyValue for Type {}

/// Type variable during inference.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeVariable(u32);
impl UnifyKey for TypeVariable {
  type Value = Option<Type>;
  fn index(&self) -> u32 {
    self.0
  }
  fn from_index(u: u32) -> Self {
    Self(u)
  }
  fn tag() -> &'static str {
    "TypeVar"
  }
}

#[derive(Debug, Clone)]
struct Constraint {
  pub file_origin: PathId,
  pub span: Span,
  pub left: Type,
  pub right: Type,
}

struct TypeInference {
  unification_table: InPlaceUnificationTable<TypeVariable>,
}
impl TypeInference {
  fn new_type_variable(&mut self) -> TypeVariable {
    self.unification_table.new_key(None)
  }

  fn infer(
    &mut self, env: ImHashMap<LocalNameId, Type>, xpr: &ValueExpr,
  ) -> (Vec<Constraint>, Type) {
    todo!()
  }
}
