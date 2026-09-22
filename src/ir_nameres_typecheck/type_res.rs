use crate::ItemId;
use crate::ast::PointerAccessKind;
use ena::unify::EqUnifyValue;
use ena::unify::UnifyKey;

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
impl Type {
  /// Does this Type contain the given TypeVariable?
  ///
  /// The check is fully recursive.
  pub fn occurs_check(&self, _v: TypeVariable) -> Result<(), Self> {
    todo!()
  }
}

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
    "TypeVariable"
  }
}
