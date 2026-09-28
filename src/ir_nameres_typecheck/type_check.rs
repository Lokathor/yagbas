#![allow(unused)]

use std::collections::hash_map::Entry;

use fnv::FnvHashMap;

use crate::FnvBiHashMap;
use crate::ItemId;
use crate::LocalNameId;
use crate::PathId;
use crate::Span;
use crate::TypeId;
use crate::ValueExprId;
use crate::YagError;
use crate::ast::Ast;
use crate::ast::Item;
use crate::ast::ItemKind;
use crate::ast::PointerAccessKind;
use crate::ast::TypeExprKind;
use crate::ast::ValueExpr;
use crate::ast::ValueExprKind;
use crate::ast::visitor::TreeVisitMut;
use crate::ir_nameres_typecheck::IrNameResTypeCheck;
use crate::ir_nameres_typecheck::Type;

#[derive(Debug, Default)]
pub struct TypeChecker {
  pub type_database: FnvBiHashMap<TypeId, Type>,
  pub item_id_to_type_id: FnvHashMap<ItemId, TypeId>,
  pub val_expr_to_type_id: FnvHashMap<ValueExprId, TypeId>,
}
impl TreeVisitMut for TypeChecker {
  fn visit_value_expr(&mut self, vx: &mut ValueExpr) {
    dbg!(&vx.id);
  }
}
