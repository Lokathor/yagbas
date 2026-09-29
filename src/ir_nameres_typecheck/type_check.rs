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
use crate::ast::Module;
use crate::ast::PointerAccessKind;
use crate::ast::Statement;
use crate::ast::StatementKind;
use crate::ast::TypeExpr;
use crate::ast::TypeExprKind;
use crate::ast::ValueExpr;
use crate::ast::ValueExprKind;
use crate::ast::visitor::TreeVisitMut;
use crate::ir_nameres_typecheck::IrNameResTypeCheck;
use crate::ir_nameres_typecheck::Type;
use crate::operators::BinOpKind;
use crate::operators::UnOpKind;

#[derive(Debug, Default)]
pub struct TypeChecker {
  pub type_database: FnvBiHashMap<TypeId, Type>,
  pub item_id_to_type_id: FnvHashMap<ItemId, TypeId>,
  pub local_id_to_type_id: FnvHashMap<LocalNameId, TypeId>,
  pub val_expr_to_type_id: FnvHashMap<ValueExprId, TypeId>,
}
impl TypeChecker {
  fn type_id_from_type(&mut self, ty: Type) -> TypeId {
    match self.type_database.get_by_right(&ty) {
      Some(id) => *id,
      None => {
        let id = TypeId::new();
        self.type_database.insert(id, ty);
        id
      }
    }
  }

  fn type_from_type_expr_kind(
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

  fn type_id_from_type_expr_kind(
    &mut self, tyx_kind: &TypeExprKind,
  ) -> Result<TypeId, ()> {
    let ty = self.type_from_type_expr_kind(tyx_kind)?;
    Ok(self.type_id_from_type(ty))
  }

  fn register_item(&mut self, item: &Item) {
    match &item.kind {
      ItemKind::StaticMmio(data) => {
        let tyx_kind = TypeExprKind::Pointer {
          target_tyx: data.tyx.clone(),
          access_kind: PointerAccessKind::Vol,
        };
        let tid = self.type_id_from_type_expr_kind(&tyx_kind).unwrap();
        self.item_id_to_type_id.insert(item.id, tid);
      }
      ItemKind::Function(data) => {
        // record arg types first.
        for arg in &data.args {
          let vx_id = arg.var.id;
          let ty_id = self.type_id_from_type_expr_kind(&arg.tyx.kind).unwrap();
          self.val_expr_to_type_id.insert(vx_id, ty_id).unwrap();
        }
        // register fn type itself.
        let arg_ids: Vec<TypeId> = data
          .args
          .iter()
          .map(|arg| self.type_id_from_type_expr_kind(&arg.tyx.kind).unwrap())
          .collect();
        let ret: TypeId = if let Some(ret_tyx) = &data.opt_ret_tyx {
          self.type_id_from_type_expr_kind(&ret_tyx.kind).unwrap()
        } else {
          self.type_id_from_type(Type::Unit)
        };
        let t = Type::Function { arg_ids, ret };
        let tid = self.type_id_from_type(t);
        self.item_id_to_type_id.insert(item.id, tid);
      }
      other => {
        dbg!(other);
      }
    }
  }
}
impl TreeVisitMut for TypeChecker {
  fn visit_module(&mut self, module: &mut Module) {
    for item in &module.items {
      self.register_item(item);
    }
  }

  fn visit_statement_vec(&mut self, statements: &mut Vec<Statement>) {
    for statement in statements {
      if let StatementKind::Item(item) = &*statement.kind {
        self.register_item(item);
      }
    }
  }

  fn visit_statement(&mut self, statement: &mut Statement) {
    if let StatementKind::Let { var, opt_tyx, opt_init } = &*statement.kind {
      todo!();
    }
  }

  fn visit_value_expr(&mut self, vx: &mut ValueExpr) {
    match &*vx.kind {
      ValueExprKind::LiteralNumber(_) => {
        let lit_id = self.type_id_from_type(Type::LiteralInteger);
        self.val_expr_to_type_id.insert(vx.id, lit_id);
      }
      ValueExprKind::NameOfStaticMmio(n) => {
        let tid = self.item_id_to_type_id.get(&n).unwrap();
        self.val_expr_to_type_id.insert(vx.id, *tid);
      }
      ValueExprKind::UnOp { op: UnOpKind::Dereference, operand } => {
        let inner_tid = self.val_expr_to_type_id.get(&operand.id).unwrap();
        let inner_ty = self.type_database.get_by_left(inner_tid).unwrap();
        let tid = match inner_ty {
          Type::Pointer { target_id, .. } => target_id,
          _ => todo!(),
        };
        self.val_expr_to_type_id.insert(vx.id, *tid);
      }
      ValueExprKind::BinOp { left, right, op: BinOpKind::Assign } => {
        let opt_right_tid = self.val_expr_to_type_id.get(&right.id).copied();
        let opt_left_tid = self.val_expr_to_type_id.get(&left.id).copied();
        match (opt_left_tid, opt_right_tid) {
          (_, None) => panic!(),
          (Some(l_tid), Some(r_tid)) => {
            // todo: check eq, coerce, error if necessary
            self.val_expr_to_type_id.insert(vx.id, l_tid);
          }
          (None, Some(tid)) => {
            self.val_expr_to_type_id.insert(left.id, tid);
            self.val_expr_to_type_id.insert(vx.id, tid);
          }
        }
      }
      ValueExprKind::Block { statements, opt_tail_vx } => match opt_tail_vx {
        Some(tail_vx) => {
          let tail_tid = self.val_expr_to_type_id.get(&tail_vx.id).unwrap();
          self.val_expr_to_type_id.insert(tail_vx.id, *tail_tid);
        }
        None => {
          let unit_id = self.type_id_from_type(Type::Unit);
          self.val_expr_to_type_id.insert(vx.id, unit_id);
        }
      },
      other => {
        dbg!(other);
      }
    }
  }
}
