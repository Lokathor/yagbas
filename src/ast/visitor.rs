use crate::{
  ast::{
    Ast, Item, ItemKind, Label, Module, Statement, TypeExpr, TypeExprKind,
    ValueExpr, ValueExprKind,
  },
  operators::BinOpKind,
};

use super::StatementKind;

pub trait AstVisitor {
  /// Visits the ast and then walks the modules.
  fn walk_ast(&mut self, ast: &Ast) {
    self.visit_ast(ast);
    for module in ast.modules.iter() {
      self.walk_module(module);
    }
  }
  /// Visits the module, then walks the items.
  fn walk_module(&mut self, module: &Module) {
    self.visit_module(module);
    for item in module.items.iter() {
      self.walk_item(item);
    }
  }
  /// Visits the item, then walks the item components.
  fn walk_item(&mut self, item: &Item) {
    self.visit_item(item);
    match &item.kind {
      ItemKind::ErrItemKind => (),
      ItemKind::Mod => (),
      ItemKind::Use { cst: _ } => (),
      ItemKind::Constant { type_decl, value_decl } => {
        self.walk_type_expr(type_decl);
        self.walk_value_expr(value_decl);
      }
      ItemKind::StaticMmio { location, type_decl } => {
        self.walk_value_expr(location);
        self.walk_type_expr(type_decl);
      }
      ItemKind::StaticRam { type_decl, init } => {
        self.walk_type_expr(type_decl);
        self.walk_value_expr(init);
      }
      ItemKind::StaticRom { type_decl, data } => {
        self.walk_type_expr(type_decl);
        self.walk_value_expr(data);
      }
      ItemKind::Function(data) => {
        if let Some(ret_tyx) = data.opt_ret_tyx.as_ref() {
          self.walk_type_expr(ret_tyx);
        }
        for arg in data.args.iter() {
          self.walk_value_expr(&arg.var);
          self.walk_type_expr(&arg.type_decl);
        }
        self.walk_value_expr(&data.body);
      }

      other => todo!("unhandled walk_item: {other:?}"),
    }
  }
  /// Walks the components before visiting the overall expression.
  fn walk_type_expr(&mut self, ty: &TypeExpr) {
    match &*ty.kind {
      TypeExprKind::Array { elem_tyx: elem_ty, elem_count } => {
        self.walk_type_expr(elem_ty);
        self.walk_value_expr(elem_count);
      }
      TypeExprKind::Pointer { target_tyx: elem_ty, access_kind: _ } => {
        self.walk_type_expr(elem_ty);
      }
      TypeExprKind::ErrTypeExprKind
      | TypeExprKind::Identifier(_)
      | TypeExprKind::NameOfStruct(_)
      | TypeExprKind::NameOfBitbag(_)
      | TypeExprKind::NameOfEnum(_)
      | TypeExprKind::Unit
      | TypeExprKind::Bool
      | TypeExprKind::U8
      | TypeExprKind::I8
      | TypeExprKind::U16
      | TypeExprKind::I16 => (),
    }
    self.visit_type_expr(ty);
  }
  /// Visits the inner expressions, then visits the whole expression.
  fn walk_value_expr(&mut self, xpr: &ValueExpr) {
    match &*xpr.kind {
      ValueExprKind::ErrValueExprKind => (),
      ValueExprKind::Identifier(_) => (),
      ValueExprKind::LiteralString(_) => (),
      ValueExprKind::LiteralNumber(_) => (),
      ValueExprKind::Block { statements } => {
        self.walk_statement_block(statements)
      }
      ValueExprKind::Loop { label, body } => {
        self.enter_label_scope(label);
        self.walk_value_expr(body);
        self.exit_label_scope(label);
      }
      ValueExprKind::While { label, condition, body } => {
        self.enter_label_scope(label);
        self.walk_value_expr(condition);
        self.walk_value_expr(body);
        self.exit_label_scope(label);
      }
      ValueExprKind::For { label, step_var, range, body } => {
        self.walk_value_expr(range);
        self.enter_label_scope(label);
        self.walk_value_expr(step_var);
        self.walk_value_expr(body);
        self.exit_label_scope(label);
      }
      ValueExprKind::If { condition, true_body, opt_false_body } => {
        self.walk_value_expr(condition);
        self.walk_value_expr(true_body);
        if let Some(false_body) = opt_false_body {
          self.walk_value_expr(false_body);
        }
      }
      ValueExprKind::BinOp { left, op, right } => {
        match op {
          // Path and access operations put the right hand side into a temporary
          // scope created by the left hand side, so we need to have special
          // handling of some kind here.
          BinOpKind::Path | BinOpKind::Access => todo!(),
          _ => {
            self.walk_value_expr(left);
            self.walk_value_expr(right);
          }
        }
      }
      ValueExprKind::UnOp { op: _, operand } => {
        self.walk_value_expr(operand);
      }
      ValueExprKind::FullRangeExclusive => (),
      ValueExprKind::FullRangeInclusive => (),
      ValueExprKind::Break { label: _, value } => {
        if let Some(xpr) = value.as_ref() {
          self.walk_value_expr(xpr);
        }
      }
      ValueExprKind::Continue { label: _ } => (),
      ValueExprKind::Call { target, args } => {
        self.walk_value_expr(target);
        for arg in args.iter() {
          self.walk_value_expr(arg);
        }
      }
      ValueExprKind::As { value, as_type } => {
        self.walk_value_expr(value);
        self.walk_type_expr(as_type);
      }
      ValueExprKind::NameOfStaticMmio(_)
      | ValueExprKind::NameOfStaticRam(_)
      | ValueExprKind::NameOfStaticRom(_)
      | ValueExprKind::NameOfConstant(_)
      | ValueExprKind::NameOfFunction(_)
      | ValueExprKind::NameOfLocalVariable(_) => (),
    }
    self.visit_value_expr(xpr);
  }
  fn walk_statement_block(&mut self, statements: &[Statement]) {
    self.enter_name_scope();
    for statement in statements.iter() {
      self.walk_statement(statement);
    }
    self.exit_name_scope();
  }
  fn walk_statement(&mut self, statement: &Statement) {
    match &*statement.kind {
      StatementKind::ErrStatementKind => (),
      StatementKind::Item(item) => {
        self.suspend_local_scopes();
        self.visit_item(item);
        self.resume_local_scopes();
      }
      StatementKind::Let { var, type_decl, initializer } => {
        if let Some(xpr) = initializer {
          self.walk_value_expr(xpr);
        }
        if let Some(ty) = type_decl {
          self.walk_type_expr(ty);
        }
        self.walk_value_expr(var);
      }
      StatementKind::Expression(value_expr) => {
        self.walk_value_expr(value_expr);
      }
    }
  }

  #[allow(unused_variables)]
  fn visit_ast(&mut self, ast: &Ast) {}
  #[allow(unused_variables)]
  fn visit_module(&mut self, module: &Module) {}
  #[allow(unused_variables)]
  fn visit_item(&mut self, item: &Item) {}
  #[allow(unused_variables)]
  fn visit_type_expr(&mut self, tyx: &TypeExpr) {}
  #[allow(unused_variables)]
  fn visit_value_expr(&mut self, xpr: &ValueExpr) {}
  #[allow(unused_variables)]
  fn visit_statement(&mut self, statement: &Statement) {}
  #[allow(unused_variables)]
  fn enter_label_scope(&mut self, opt_label: &Option<Label>) {}
  #[allow(unused_variables)]
  fn exit_label_scope(&mut self, opt_label: &Option<Label>) {}
  fn enter_name_scope(&mut self) {}
  fn exit_name_scope(&mut self) {}
  fn enter_local_scope(&mut self) {}
  fn exit_local_scope(&mut self) {}
  fn suspend_local_scopes(&mut self) {}
  fn resume_local_scopes(&mut self) {}
}
