#![allow(unused)]

use std::collections::hash_map::Entry;

use crate::ItemId;
use crate::LocalNameId;
use crate::PathId;
use crate::Span;
use crate::TypeId;
use crate::YagError;
use crate::ast::Ast;
use crate::ast::Item;
use crate::ast::ItemKind;
use crate::ast::PointerAccessKind;
use crate::ast::TypeExprKind;
use crate::ast::ValueExpr;
use crate::ast::ValueExprKind;
use crate::ir_nameres_typecheck::IrNameResTypeCheck;
use crate::ir_nameres_typecheck::PRIMITIVE_TYPE_NAMES;
use crate::ir_nameres_typecheck::Type;

pub fn populate_basic_types(ir: &mut IrNameResTypeCheck) {
  ir.type_database.insert(TypeId::new(), Type::MagicIntegerLiteral);
  for name in PRIMITIVE_TYPE_NAMES {
    ir.type_database.insert(TypeId::new(), Type::SimplePrimitive(name));
  }
}

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
    TypeExprKind::Simple(s) => unimplemented!(),
  }
}

pub fn compute_types_of_ir(ir: &mut IrNameResTypeCheck) {
  let _bool_id: TypeId = ir
    .type_database
    .get_by_right(&Type::SimplePrimitive("bool"))
    .copied()
    .unwrap();
  let u16_id: TypeId = ir
    .type_database
    .get_by_right(&Type::SimplePrimitive("u16"))
    .copied()
    .unwrap();
  let mut modules = core::mem::take(&mut ir.ast.modules);
  for module in modules.iter() {
    for item in module.items.iter() {
      match &item.kind {
        ItemKind::StaticMmio { location, type_decl } => {
          spread_value_expr(ir, location);
          check_value_expr(ir, location, u16_id);
        }
        ItemKind::Constant { type_decl, value_decl } => {
          spread_value_expr(ir, value_decl);
          let target_type = type_from_type_expr_kind(ir, &type_decl.kind);
          let target_id =
            ir.type_database.get_by_right(&target_type).copied().unwrap();
          check_value_expr(ir, value_decl, target_id);
        }
        #[cfg(false)]
        ItemKind::Function { args, ret_ty, statements } => {
          for arg in args {
            todo!()
          }
          todo!("statements");
          let target_type = type_from_type_expr_kind(ir, &ret_ty.kind);
          let ret_ty_id =
            ir.type_database.get_by_right(&target_type).copied().unwrap();
          check_value_expr(ir, todo!(), ret_ty_id);
        }
        other => todo!("compute_types_of_ir: {other:?}"),
      }
    }
  }
  core::mem::replace(&mut ir.ast.modules, modules);
}

fn check_value_expr(
  ir: &mut IrNameResTypeCheck, xpr: &ValueExpr, goal_id: TypeId,
) {
  static APPROVED_COERCION_TARGETS: &[Type] = &[
    Type::SimplePrimitive("u8"),
    Type::SimplePrimitive("i8"),
    Type::SimplePrimitive("u16"),
    Type::SimplePrimitive("i16"),
  ];
  match ir.expr_types.entry(xpr.id) {
    Entry::Occupied(oe) => {
      let actual_id = *oe.get();
      if actual_id != goal_id {
        let actual_type: &Type =
          ir.type_database.get_by_left(&actual_id).unwrap();
        let goal_type: &Type = ir.type_database.get_by_left(&goal_id).unwrap();
        if (actual_type == &Type::MagicIntegerLiteral)
          && (APPROVED_COERCION_TARGETS.contains(goal_type))
        {
          *oe.into_mut() = goal_id;
          match &*xpr.kind {
            ValueExprKind::LiteralNumber(_) => (),
            other => {
              todo!("re-run check_has_type on sub-expressions: {other:?}")
            }
          }
        } else {
          eprintln!("type error, wanted {goal_id:?} got {actual_id:?}");
        }
      }
    }
    Entry::Vacant(_) => todo!(),
  }
}

fn spread_value_expr(ir: &mut IrNameResTypeCheck, xpr: &ValueExpr) {
  match &*xpr.kind {
    ValueExprKind::LiteralNumber(x) => {
      match ir.expr_types.entry(xpr.id) {
        Entry::Vacant(ve) => {
          // TODO: check the literal for a suffix and use that if it's present.
          let num_lit_id = ir
            .type_database
            .get_by_right(&Type::MagicIntegerLiteral)
            .copied()
            .unwrap();
          ve.insert(num_lit_id);
        }
        Entry::Occupied(_) => (),
      };
    }
    other => todo!("spread_types: {other:?}"),
  }
}
