//! "Grant me the power of name resolution!"

use crate::ast::Ast;
use crate::ir_nameres_typecheck::type_check::Type;
use crate::{TypeId, ValueExprId};

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
