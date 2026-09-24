use crate::ValueExprId;
use crate::ir_nameres_typecheck::type_res::Type;
use crate::{
  LabelId, LocalNameId, Span, YagError,
  ast::{
    Ast, Item, ItemKind, Label, LabelKind, PatternKind, Statement,
    StatementKind, TypeExpr, TypeExprKind, ValueExpr,
    ValueExprKind::{self},
  },
};
use std::collections::HashMap;

pub mod type_res;

#[derive(Debug, Clone)]
pub struct IrNameResTypeCheck {
  pub ast: Ast,
  pub expr_types: HashMap<ValueExprId, Type>,
}

pub type VarNameScopes = Vec<HashMap<String, ValueExprKind>>;
pub type TypeNameScopes = Vec<HashMap<String, TypeExprKind>>;
pub type LabelNameScopes = Vec<HashMap<String, LabelKind>>;

#[derive(Debug, Clone, Default)]
pub struct ResolverContext {
  pub var_name_scopes: VarNameScopes,
  pub type_name_scopes: TypeNameScopes,
  pub label_name_scopes: LabelNameScopes,
  pub errors: Vec<YagError>,
}
impl ResolverContext {
  pub fn push_scope(&mut self) {
    self.var_name_scopes.push(HashMap::default());
    self.type_name_scopes.push(HashMap::default());
    self.label_name_scopes.push(HashMap::default());
  }
  pub fn pop_scope(&mut self) {
    self.var_name_scopes.pop();
    self.type_name_scopes.pop();
    self.label_name_scopes.pop();
  }
  pub fn within_scope<F>(&mut self, mut f: F)
  where
    F: FnOnce(&mut ResolverContext),
  {
    self.push_scope();
    f(self);
    self.pop_scope();
  }

  pub fn lookup_var_name(&self, name: &str) -> Option<&ValueExprKind> {
    self.var_name_scopes.iter().rev().filter_map(|hm| hm.get(name)).next()
  }
  pub fn lookup_type_name(&self, ty: &str) -> Option<&TypeExprKind> {
    self.type_name_scopes.iter().rev().filter_map(|hm| hm.get(ty)).next()
  }
  /// Label lookups have the weird property where an empty input has to still
  /// match on the most recent label (the innermost label), even if that
  /// innermost label has a non-empty name.
  pub fn lookup_label_name(&self, label: &str) -> Option<&LabelKind> {
    if label.is_empty() {
      self
        .label_name_scopes
        .iter()
        .rev()
        .filter_map(|hm| hm.values().next())
        .next()
    } else {
      self.label_name_scopes.iter().rev().filter_map(|hm| hm.get(label)).next()
    }
  }

  pub fn register_var_name(
    &mut self, name: String, replacement: ValueExprKind,
  ) -> Option<ValueExprKind> {
    self.var_name_scopes.last_mut().unwrap().insert(name, replacement)
  }
  pub fn register_type_name(
    &mut self, name: String, replacement: TypeExprKind,
  ) -> Option<TypeExprKind> {
    self.type_name_scopes.last_mut().unwrap().insert(name, replacement)
  }
  pub fn register_label_name(
    &mut self, name: String, replacement: LabelKind,
  ) -> Option<LabelKind> {
    // there should only ever be a single label at any given scope point.
    debug_assert!(self.label_name_scopes.last_mut().unwrap().is_empty());
    self.label_name_scopes.last_mut().unwrap().insert(name, replacement)
  }
}

pub fn do_names(ir: &mut IrNameResTypeCheck) {
  let mut ctx = ResolverContext::default();
  ctx.push_scope();
  for primitive in ["()", "bool", "u8", "i8", "u16", "i16"] {
    ctx.register_type_name(
      primitive.to_owned(),
      TypeExprKind::NameOfPrimitive(primitive),
    );
  }

  for module in ir.ast.modules.iter_mut() {
    ctx.within_scope(|ctx| {
      let mut items_defined_this_scope = Vec::new();
      for item in module.items.iter() {
        if items_defined_this_scope.contains(&item.name.as_str()) {
          // todo: error about multiple definitions
          continue;
        } else {
          items_defined_this_scope.push(item.name.as_str());
        }
        register_item_definition_info(ctx, item);
      }

      for item in module.items.iter_mut() {
        do_names_in_item(ctx, item);
      }
    });
  }
}

fn register_item_definition_info(ctx: &mut ResolverContext, item: &Item) {
  let name = item.name.clone();
  match &item.kind {
    ItemKind::Constant { .. } => {
      let replacement = ValueExprKind::NameOfConstant(item.id);
      ctx.register_var_name(name, replacement);
    }
    ItemKind::StaticMmio { .. } => {
      let replacement = ValueExprKind::NameOfStaticMmio(item.id);
      ctx.register_var_name(name, replacement);
    }
    ItemKind::StaticRam { .. } => {
      let replacement = ValueExprKind::NameOfStaticRam(item.id);
      ctx.register_var_name(name, replacement);
    }
    ItemKind::StaticRom { .. } => {
      let replacement = ValueExprKind::NameOfStaticRom(item.id);
      ctx.register_var_name(name, replacement);
    }
    ItemKind::Function { .. } => {
      let replacement = ValueExprKind::NameOfFunction(item.id);
      ctx.register_var_name(name, replacement);
    }
    ItemKind::Struct { .. } => {
      // todo: register the type id itself somewhere?
      let replacement = TypeExprKind::NameOfStruct(item.id);
      ctx.register_type_name(name, replacement);
    }
    ItemKind::Bitbag { .. } => {
      // todo: register the type id itself somewhere?
      let replacement = TypeExprKind::NameOfBitbag(item.id);
      ctx.register_type_name(name, replacement);
    }
    ItemKind::Enum { .. } => {
      // todo: register the type id itself somewhere?
      let replacement = TypeExprKind::NameOfEnum(item.id);
      ctx.register_type_name(name, replacement);
    }
    ItemKind::ErrItemKind => return,
    other => todo!("unknown how to register {other:?}"),
  };
}

fn do_names_in_item(ctx: &mut ResolverContext, item: &mut Item) {
  match &mut item.kind {
    ItemKind::StaticMmio { location, type_decl } => {
      do_names_in_value_expr(ctx, location);
      do_names_in_type_expr(ctx, type_decl);
    }
    ItemKind::Constant { type_decl, value_decl } => {
      do_names_in_type_expr(ctx, type_decl);
      do_names_in_value_expr(ctx, value_decl);
    }
    ItemKind::Function { args, ret_ty, statements } => {
      do_names_in_type_expr(ctx, ret_ty);
      for arg in args.iter_mut() {
        do_names_in_type_expr(ctx, &mut arg.type_decl);
      }
      ctx.within_scope(|ctx| {
        for arg in args.iter_mut() {
          match &mut arg.pattern.kind {
            PatternKind::Simple(name, val_id) => {
              let id = LocalNameId::new();
              let name = name.clone();
              let replacement = ValueExprKind::NameOfLocalVariable(id);
              let _ = ctx.register_var_name(name, replacement);
              arg.pattern.kind = PatternKind::SimpleLocalName(id, *val_id);
            }
            other => todo!("unhandled pattern kind: {other:?}"),
          }
        }
        do_names_in_body(ctx, statements);
      });
    }
    other => todo!("unhandled inside item: {other:?}"),
  }
}

fn do_names_in_body(
  ctx: &mut ResolverContext, statements: &mut Vec<Statement>,
) {
  ctx.within_scope(|ctx| {
    let mut items_defined_this_scope = Vec::new();
    for statement in statements.iter() {
      match &*statement.kind {
        StatementKind::Item(item) => {
          if items_defined_this_scope.contains(&item.name.as_str()) {
            // todo: error about multiple definitions
            continue;
          } else {
            items_defined_this_scope.push(item.name.as_str());
          }
          register_item_definition_info(ctx, item);
        }
        _ => continue,
      }
    }
    for statement in statements.iter_mut() {
      do_names_in_statement(ctx, statement);
    }
  });
}

fn do_names_in_statement(ctx: &mut ResolverContext, statement: &mut Statement) {
  match &mut *statement.kind {
    StatementKind::Let { pattern, type_decl, initializer } => {
      if let Some(xpr) = initializer {
        do_names_in_value_expr(ctx, xpr);
      }
      if let Some(ty) = type_decl {
        do_names_in_type_expr(ctx, ty);
      }
      match &mut pattern.kind {
        PatternKind::Simple(name, val_id) => {
          let id = LocalNameId::new();
          let name = name.clone();
          let replacement = ValueExprKind::NameOfLocalVariable(id);
          let _ = ctx.register_var_name(name, replacement);
          pattern.kind = PatternKind::SimpleLocalName(id, *val_id);
        }
        other => todo!("unhandled let pattern kind: {other:?}"),
      }
    }
    StatementKind::Expression(xpr) => do_names_in_value_expr(ctx, xpr),
    StatementKind::Item(item) => do_names_in_item(ctx, item),
    StatementKind::ErrStatementKind => return,
  }
}

fn do_names_in_value_expr(ctx: &mut ResolverContext, xpr: &mut ValueExpr) {
  match &mut *xpr.kind {
    ValueExprKind::Identifier(name) => {
      if let Some(replacement) = ctx.lookup_var_name(name.as_str()) {
        *xpr.kind = replacement.clone();
      } else {
        todo!()
      }
    }
    ValueExprKind::LiteralNumber(_) => {
      // todo: if the type has a suffix we could assign a type right here.
    }
    ValueExprKind::Loop { label, statements } => {
      ctx.within_scope(|ctx| {
        if let Some(label) = label {
          match &mut label.kind {
            LabelKind::Identifier(name) => {
              let id = LabelId::new();
              let name = name.clone();
              let replacement = LabelKind::IdNum(id);
              let _ = ctx.register_label_name(name, replacement);
              label.kind = LabelKind::IdNum(id);
            }
            other => todo!("unhandled let pattern kind: {other:?}"),
          }
        } else {
          let id = LabelId::new();
          let name = String::from("");
          let replacement = LabelKind::IdNum(id);
          let _ = ctx.register_label_name(name, replacement);
          *label =
            Some(Label { span: Span::default(), kind: LabelKind::IdNum(id) });
        }
        do_names_in_body(ctx, statements);
      });
    }
    ValueExprKind::For { label, step_var, range, statements } => {
      do_names_in_value_expr(ctx, range);
      ctx.within_scope(|ctx| {
        if let Some(label) = label {
          match &mut label.kind {
            LabelKind::Identifier(name) => {
              let id = LabelId::new();
              let name = name.clone();
              let replacement = LabelKind::IdNum(id);
              let _ = ctx.register_label_name(name, replacement);
              label.kind = LabelKind::IdNum(id);
            }
            other => todo!("unhandled let pattern kind: {other:?}"),
          }
        } else {
          let id = LabelId::new();
          let name = String::from("");
          let replacement = LabelKind::IdNum(id);
          let _ = ctx.register_label_name(name, replacement);
          *label =
            Some(Label { span: Span::default(), kind: LabelKind::IdNum(id) });
        }
        match &mut step_var.kind {
          PatternKind::Simple(name, val_id) => {
            let id = LocalNameId::new();
            let name = name.clone();
            let replacement = ValueExprKind::NameOfLocalVariable(id);
            let _ = ctx.register_var_name(name, replacement);
            step_var.kind = PatternKind::SimpleLocalName(id, *val_id);
          }
          other => todo!("unhandled pattern kind: {other:?}"),
        }
        do_names_in_body(ctx, statements);
      });
    }
    ValueExprKind::Break { label, value } => {
      match label {
        Some(label_inner) => match &mut label_inner.kind {
          LabelKind::Identifier(l) => {
            if let Some(replacement) = ctx.lookup_label_name(l.as_str()) {
              label_inner.kind = replacement.clone();
            } else {
              todo!()
            }
          }
          _ => todo!("unhadnled label kind in break expr"),
        },
        None => {
          if let Some(replacement) = ctx.lookup_label_name("") {
            *label =
              Some(Label { span: Span::default(), kind: replacement.clone() });
          } else {
            todo!()
          }
        }
      }
      if let Some(xpr) = value {
        do_names_in_value_expr(ctx, xpr);
      }
    }
    ValueExprKind::If { condition, when_true, when_false } => {
      do_names_in_value_expr(ctx, condition);
      do_names_in_body(ctx, when_true);
      do_names_in_body(ctx, when_false);
    }
    ValueExprKind::BinOp { left, op: _, right } => {
      // todo: this is wrong for FieldAccess ops. when the left side is field accessable, the right side is a field name not a general variable name.
      // todo: also Path ops.
      do_names_in_value_expr(ctx, left);
      do_names_in_value_expr(ctx, right);
    }
    ValueExprKind::UnOp { op: _, operand } => {
      do_names_in_value_expr(ctx, operand);
    }
    other => todo!("unhandled inside value expression: {other:?}"),
  }
}
fn do_names_in_type_expr(ctx: &mut ResolverContext, ty: &mut TypeExpr) {
  match &mut *ty.kind {
    TypeExprKind::Simple(t) => {
      if let Some(replacement) = ctx.lookup_type_name(t.as_str()) {
        *ty.kind = replacement.clone();
      } else {
        todo!()
      }
    }
    TypeExprKind::Array { elem_ty, elem_count } => {
      do_names_in_type_expr(ctx, elem_ty);
      do_names_in_value_expr(ctx, elem_count);
    }
    other => todo!("unhandled inside type expression: {other:?}"),
  }
}
