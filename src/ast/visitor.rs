use crate::{
  ast::{
    Ast, Item, ItemKind, Label, Module, Pattern, Statement, TypeExpr,
    TypeExprKind, ValueExpr, ValueExprKind,
  },
  operators::BinOpKind,
};

use super::StatementKind;

#[allow(unused_variables)]
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
      ItemKind::Function { args, ret_ty, statements } => {
        self.walk_type_expr(ret_ty);
        for arg in args.iter() {
          self.visit_pattern(&arg.pattern);
          self.walk_type_expr(&arg.type_decl);
        }
        self.walk_statement_block(statements);
      }

      other => todo!("unhandled walk_item: {other:?}"),
    }
  }
  /// Walks the components before visiting the overall expression.
  fn walk_type_expr(&mut self, ty: &TypeExpr) {
    match &*ty.kind {
      TypeExprKind::Array { elem_ty, elem_count } => {
        self.walk_type_expr(elem_ty);
        self.walk_value_expr(elem_count);
      }
      TypeExprKind::Pointer { elem_ty, access_kind } => {
        self.walk_type_expr(elem_ty);
      }
      TypeExprKind::ErrTypeExprKind
      | TypeExprKind::Simple(_)
      | TypeExprKind::NameOfStruct(_)
      | TypeExprKind::NameOfBitbag(_)
      | TypeExprKind::NameOfEnum(_)
      | TypeExprKind::NameOfPrimitive(_) => (),
    }
    self.visit_type_expr(ty);
  }
  ///
  fn walk_value_expr(&mut self, xpr: &ValueExpr) {
    match &*xpr.kind {
      ValueExprKind::ErrValueExprKind => (),
      ValueExprKind::Identifier(_) => (),
      ValueExprKind::LiteralString(_) => (),
      ValueExprKind::LiteralNumber(_) => (),
      ValueExprKind::Block { statements } => {
        self.walk_statement_block(statements)
      }
      ValueExprKind::Loop { label, statements } => {
        self.enter_label_scope(label);
        self.walk_statement_block(statements);
        self.exit_label_scope(label);
      }
      ValueExprKind::While { label, condition, statements } => {
        self.enter_label_scope(label);
        self.walk_value_expr(condition);
        self.walk_statement_block(statements);
        self.exit_label_scope(label);
      }
      ValueExprKind::For { label, step_var, range, statements } => {
        self.walk_value_expr(range);
        self.enter_label_scope(label);
        self.visit_pattern(step_var);
        self.walk_statement_block(statements);
        self.exit_label_scope(label);
      }
      ValueExprKind::If { condition, when_true, when_false } => {
        self.walk_value_expr(condition);
        self.walk_statement_block(when_true);
        self.walk_statement_block(when_false);
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
      ValueExprKind::UnOp { op, operand } => {
        self.walk_value_expr(operand);
      }
      ValueExprKind::FullRangeExclusive => (),
      ValueExprKind::FullRangeInclusive => (),
      ValueExprKind::Break { label, value } => {
        if let Some(xpr) = value.as_ref() {
          self.walk_value_expr(xpr);
        }
      }
      ValueExprKind::Continue { label } => (),
      ValueExprKind::Call { target, args } => {
        self.walk_value_expr(target);
        for arg in args.iter() {
          self.walk_value_expr(xpr);
        }
      }
      ValueExprKind::As { value, as_type } => {
        self.walk_value_expr(value);
        self.walk_type_expr(as_type);
      }
      ValueExprKind::NameOfStaticMmio(item_id) => (),
      ValueExprKind::NameOfStaticRam(item_id) => (),
      ValueExprKind::NameOfStaticRom(item_id) => (),
      ValueExprKind::NameOfConstant(item_id) => (),
      ValueExprKind::NameOfFunction(item_id) => (),
      ValueExprKind::NameOfLocalVariable(local_name_id) => (),
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
      StatementKind::Let { pattern, type_decl, initializer } => {
        if let Some(xpr) = initializer {
          self.walk_value_expr(xpr);
        }
        if let Some(ty) = type_decl {
          self.walk_type_expr(ty);
        }
        self.visit_pattern(pattern);
      }
      StatementKind::Expression(value_expr) => {
        self.walk_value_expr(value_expr);
      }
    }
  }

  fn visit_ast(&mut self, ast: &Ast) {}
  fn visit_module(&mut self, module: &Module) {}
  fn visit_item(&mut self, item: &Item) {}
  fn visit_type_expr(&mut self, ty: &TypeExpr) {}
  fn visit_value_expr(&mut self, xpr: &ValueExpr) {}
  fn visit_pattern(&mut self, pattern: &Pattern) {}
  fn visit_statement(&mut self, statement: &Statement) {}
  fn enter_label_scope(&mut self, opt_label: &Option<Label>) {}
  fn exit_label_scope(&mut self, opt_label: &Option<Label>) {}
  fn enter_name_scope(&mut self) {}
  fn exit_name_scope(&mut self) {}
  fn enter_local_scope(&mut self) {}
  fn exit_local_scope(&mut self) {}
  fn suspend_local_scopes(&mut self) {}
  fn resume_local_scopes(&mut self) {}
}
