use crate::ast::{Ast, Item, ItemKind, Module, TypeExpr, ValueExpr};

#[allow(unused_variables)]
pub trait AstVisitor {
  fn walk_ast(&mut self, ast: &Ast) {
    for module in ast.modules.iter() {
      self.walk_module(module);
    }
  }
  fn walk_module(&mut self, module: &Module) {
    self.visit_module(module);
    for item in module.items.iter() {
      self.walk_item(item);
    }
  }
  fn walk_item(&mut self, item: &Item) {
    self.visit_item(item);
    match &item.kind {
      ItemKind::ErrItemKind => (),
      ItemKind::Constant { type_decl, value_decl } => {
        self.walk_type_expr(type_decl);
        self.walk_value_expr(value_decl);
      }
      other => todo!("unhandled walk_ast: {other:?}"),
    }
  }
  fn walk_type_expr(&mut self, ty: &TypeExpr) {
    todo!()
  }
  fn walk_value_expr(&mut self, xpr: &ValueExpr) {
    todo!()
  }

  fn visit_module(&mut self, module: &Module) {}
  fn visit_item(&mut self, item: &Item) {}
  fn visit_type_expr(&mut self, ty: &TypeExpr) {}
  fn visit_value_expr(&mut self, xpr: &ValueExpr) {}
}
