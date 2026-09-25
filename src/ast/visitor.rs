use crate::{
  ast::{
    Ast, Item, Label, Module, Statement, TypeExpr, ValueExpr, ValueExprKind,
  },
  operators::BinOpKind,
};

use super::{ItemKind, TypeExprKind};

/// Allows for visiting important parts of an [Ast] in the "tree" ordering.
///
/// * All methods have a default impl, so no methods are required.
/// * To make your visitor "do something", override the `visit` methods.
/// * You *probably* should not override the `walk` methods, which use a default
///   walk ordering that should be suitable for most purposes.
pub trait TreeVisitMut {
  /// Visits the whole Ast, then walks the modules.
  fn walk_ast(&mut self, ast: &mut Ast) {
    self.visit_ast(ast);
    for module in &mut ast.modules {
      self.walk_module(module);
    }
  }

  /// Visits the whole module, then visits each item.
  fn walk_module(&mut self, module: &mut Module) {
    self.visit_module(module);
    for item in &mut module.items {
      self.walk_item(item);
    }
  }

  /// Visits the whole item, then walks the item components.
  fn walk_item(&mut self, item: &mut Item) {
    self.visit_item(item);
    match &mut item.kind {
      ItemKind::ErrItemKind => (),
      ItemKind::Constant(data) => {
        self.visit_type_expr(&mut data.tyx);
        self.visit_value_expr(&mut data.vx);
      }
      ItemKind::StaticMmio(data) => {
        self.visit_value_expr(&mut data.location);
        self.visit_type_expr(&mut data.tyx);
      }
      ItemKind::StaticRam(data) => {
        self.visit_type_expr(&mut data.tyx);
        self.visit_value_expr(&mut data.init);
      }
      ItemKind::StaticRom(data) => {
        self.visit_type_expr(&mut data.tyx);
        self.visit_value_expr(&mut data.vx);
      }
      ItemKind::Function(data) => {
        debug_assert!(matches!(&*data.body.kind, ValueExprKind::Block { .. }));
        for arg in &mut data.args {
          self.register_block_local(&mut arg.var);
          self.walk_type_expr(&mut arg.tyx);
        }
        if let Some(ret_tyx) = &mut data.opt_ret_tyx {
          self.walk_type_expr(ret_tyx);
        }
        self.walk_value_expr(&mut data.body);
      }
      ItemKind::Struct(data) => {
        for field in &mut data.fields {
          self.walk_type_expr(&mut field.tyx);
        }
      }
      ItemKind::Bitbag(data) => {
        for field in &mut data.fields {
          self.walk_value_expr(&mut field.bit);
        }
      }
      ItemKind::Enum(_) => (),
      ItemKind::Impl(_) => {
        todo!(
          "each impl item visit really wants to know the impl target,
          so normal item visiting isn't a good fit.
          maybe a separate method?
          I guess it's not important right now."
        );
      }
      ItemKind::Use(_) => (),
      ItemKind::Mod => (),
    }
  }

  /// Visits the whole list, then visits the elements of the list.
  fn walk_statement_vec(&mut self, statements: &mut Vec<Statement>) {
    self.visit_statement_vec(statements);
    for statement in statements {
      self.visit_statement(statement);
    }
  }

  /// Recursively visits the **components first**, then the expression itself.
  ///
  /// ## Special Case
  /// * When a `BinOp` has a `BinOpKind` of `Access` or `Path`, the right side
  ///   expression is effectively "scoped" to the left side's type, and so the
  ///   right side is not automatically recursed into before visiting the entire
  ///   expression.
  fn walk_value_expr(&mut self, vx: &mut ValueExpr) {
    match &mut *vx.kind {
      ValueExprKind::ErrValueExprKind => (),
      ValueExprKind::Identifier(_) => (),
      ValueExprKind::LiteralString(_) => (),
      ValueExprKind::LiteralNumber(_) => (),
      ValueExprKind::Block { statements } => {
        self.walk_statement_vec(statements);
      }
      ValueExprKind::Loop { opt_label, body } => {
        debug_assert!(matches!(&*body.kind, ValueExprKind::Block { .. }));
        self.push_label_point(opt_label);
        self.walk_value_expr(body);
        self.pop_label_point(opt_label);
      }
      ValueExprKind::While { opt_label, condition, body } => {
        debug_assert!(matches!(&*body.kind, ValueExprKind::Block { .. }));
        self.push_label_point(opt_label);
        self.walk_value_expr(condition);
        self.walk_value_expr(body);
        self.pop_label_point(opt_label);
      }
      ValueExprKind::For { opt_label, step_var, range, body } => {
        debug_assert!(matches!(&*body.kind, ValueExprKind::Block { .. }));
        self.walk_value_expr(range);
        self.push_label_point(opt_label);
        self.register_block_local(step_var);
        self.walk_value_expr(body);
        self.pop_label_point(opt_label);
      }
      ValueExprKind::If { condition, true_body, opt_false_body } => {
        debug_assert!(matches!(&*true_body.kind, ValueExprKind::Block { .. }));
        debug_assert!(matches!(
          opt_false_body.as_ref().map(|f| &*f.kind),
          Some(&ValueExprKind::Block { .. })
        ));
        self.walk_value_expr(condition);
        self.walk_value_expr(true_body);
        if let Some(false_body) = opt_false_body {
          self.walk_value_expr(false_body);
        }
      }
      ValueExprKind::BinOp { left, op: BinOpKind::Access, right: _ } => {
        self.walk_value_expr(left);
      }
      ValueExprKind::BinOp { left, op: BinOpKind::Path, right: _ } => {
        self.walk_value_expr(left);
      }
      ValueExprKind::BinOp { left, op: _, right } => {
        self.walk_value_expr(left);
        self.walk_value_expr(right);
      }
      ValueExprKind::UnOp { op: _, operand } => {
        self.walk_value_expr(operand);
      }
      ValueExprKind::FullRangeExclusive => (),
      ValueExprKind::FullRangeInclusive => (),
      ValueExprKind::Break { opt_label, opt_vx } => {
        self.visit_opt_label(opt_label);
        if let Some(vx) = opt_vx {
          self.walk_value_expr(vx);
        }
      }
      ValueExprKind::Continue { opt_label } => {
        self.visit_opt_label(opt_label);
      }
      ValueExprKind::Call { target, args } => {
        self.walk_value_expr(target);
        for arg in args {
          self.walk_value_expr(arg);
        }
      }
      ValueExprKind::As { value, as_type } => {
        self.walk_value_expr(value);
        self.walk_type_expr(as_type);
      }
      ValueExprKind::NameOfStaticMmio(_) => (),
      ValueExprKind::NameOfStaticRam(_) => (),
      ValueExprKind::NameOfStaticRom(_) => (),
      ValueExprKind::NameOfConstant(_) => (),
      ValueExprKind::NameOfFunction(_) => (),
      ValueExprKind::NameOfLocalVariable(_) => (),
    }
    self.visit_value_expr(vx);
  }
  /// Recursively visits the **components first**, then the expression itself.
  fn walk_type_expr(&mut self, tyx: &mut TypeExpr) {
    match &mut *tyx.kind {
      TypeExprKind::ErrTypeExprKind => (),
      TypeExprKind::Identifier(_) => (),
      TypeExprKind::Array { elem_tyx, elem_count } => {
        self.walk_type_expr(elem_tyx);
        self.walk_value_expr(elem_count);
      }
      TypeExprKind::Pointer { target_tyx, access_kind: _ } => {
        self.walk_type_expr(target_tyx);
      }
      TypeExprKind::NameOfStruct(_) => (),
      TypeExprKind::NameOfBitbag(_) => (),
      TypeExprKind::NameOfEnum(_) => (),
      TypeExprKind::Unit => (),
      TypeExprKind::Bool => (),
      TypeExprKind::U8 => (),
      TypeExprKind::I8 => (),
      TypeExprKind::U16 => (),
      TypeExprKind::I16 => (),
    }
    self.visit_type_expr(tyx);
  }

  #[allow(unused_variables)]
  fn visit_ast(&mut self, ast: &mut Ast) {}

  #[allow(unused_variables)]
  fn visit_module(&mut self, module: &mut Module) {}

  #[allow(unused_variables)]
  fn visit_item(&mut self, item: &mut Item) {}

  #[allow(unused_variables)]
  fn visit_statement_vec(&mut self, statements: &mut Vec<Statement>) {}

  #[allow(unused_variables)]
  fn visit_statement(&mut self, statement: &mut Statement) {}

  #[allow(unused_variables)]
  fn visit_value_expr(&mut self, vx: &mut ValueExpr) {}

  #[allow(unused_variables)]
  fn visit_type_expr(&mut self, tyx: &mut TypeExpr) {}

  #[allow(unused_variables)]
  fn visit_opt_label(&mut self, opt_label: &mut Option<Label>) {}

  #[allow(unused_variables)]
  fn push_label_point(&mut self, opt_label: &mut Option<Label>) {}

  #[allow(unused_variables)]
  fn pop_label_point(&mut self, opt_label: &mut Option<Label>) {}

  #[allow(unused_variables)]
  fn push_block_point(&mut self, opt_label: &mut Option<Label>) {}

  /// Notify the walker of a local new in the current block.
  ///
  /// This is distinct from the `visit_value_expr` because it overrides
  /// ("shadows") any previous definition of the identifier in this block.
  #[allow(unused_variables)]
  fn register_block_local(&mut self, vx: &mut ValueExpr) {}

  #[allow(unused_variables)]
  fn pop_block_point(&mut self, opt_label: &mut Option<Label>) {}
}
