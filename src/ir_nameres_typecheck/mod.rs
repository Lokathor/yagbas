use fnv::FnvHashMap;

use crate::{Span, ast::Ast, make_global_id, path_id::PathId};

make_global_id!(
  /// Identifier for a particular item name.
  NameId
);

make_global_id!(
  /// Identifier for a particular type.
  TypeId
);

#[derive(Debug, Clone, Copy)]
pub struct NameInfo {
  pub decl_file: PathId,
  pub decl_span: Span,
  pub decl_type: TypeId,
  pub item_kind: ItemKind,
}

#[derive(Debug, Clone, Copy)]
pub enum ItemKind {
  Constant,
  StaticMmio,
  StaticRam,
  StaticRom,
  Function,
  Struct,
  Bitbag,
  Enum,
}

#[derive(Debug, Clone)]
pub struct TypeInfo {
  pub decl_file: PathId,
  pub decl_span: Span,
  pub kind: TypeKind,
}

#[derive(Debug, Clone)]
pub enum TypeKind {
  Simple(String),
  Array { elem_ty: TypeId, elem_count: usize },
  ConstPtr { elem_ty: TypeId },
  MutPtr { elem_ty: TypeId },
  VolPtr { elem_ty: TypeId },
  Function { args: Vec<TypeId>, ret: TypeId },
}

/// Intermediate Representation: Names are resolved and Types are checked.
#[derive(Debug, Clone)]
pub struct IrNameResTypeCheck {
  pub ast: Ast,
  /// Table of all non-local-variable names in the program.
  pub names: FnvHashMap<NameId, NameInfo>,
  /// Table of all types that get instanciated in the program.
  pub types: FnvHashMap<TypeId, TypeInfo>,
  pub errors: Vec<NameResTypeCheckError>,
}

#[derive(Debug, Clone)]
pub struct NameResTypeCheckError {
  pub file_origin: PathId,
  pub span: Span,
  pub message: String,
}
