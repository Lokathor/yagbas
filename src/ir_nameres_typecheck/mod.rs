use fnv::FnvHashMap;

use crate::{
  Span, YagError,
  ast::{
    Ast, Item, ItemKind, Label, LabelId, LabelKind, LocalNameId, PatternKind,
    Statement, StatementKind, TypeExpr, TypeExprKind, ValueExpr,
    ValueExprKind::{self},
  },
};

#[derive(Debug, Clone)]
pub struct IrNameResTypeCheck {
  pub ast: Ast,
}

pub type NameScopes = Vec<FnvHashMap<String, ValueExprKind>>;
pub type TypeScopes = Vec<FnvHashMap<String, TypeExprKind>>;
pub type LabelScopes = Vec<FnvHashMap<String, LabelKind>>;

#[derive(Debug, Clone, Default)]
pub struct ResolverContext {
  pub name_scopes: NameScopes,
  pub type_scopes: TypeScopes,
  pub label_scopes: LabelScopes,
  pub errors: Vec<YagError>,
}
impl ResolverContext {
  pub fn push_scope(&mut self) {
    self.name_scopes.push(FnvHashMap::default());
    self.type_scopes.push(FnvHashMap::default());
    self.label_scopes.push(FnvHashMap::default());
  }
  pub fn pop_scope(&mut self) {
    self.name_scopes.pop();
    self.type_scopes.pop();
    self.label_scopes.pop();
  }
  pub fn within_scope<F>(&mut self, mut f: F)
  where
    F: FnOnce(&mut ResolverContext),
  {
    self.push_scope();
    f(self);
    self.pop_scope();
  }

  pub fn lookup_name(&self, name: &str) -> Option<&ValueExprKind> {
    self.name_scopes.iter().rev().filter_map(|hm| hm.get(name)).next()
  }
  pub fn lookup_type(&self, ty: &str) -> Option<&TypeExprKind> {
    self.type_scopes.iter().rev().filter_map(|hm| hm.get(ty)).next()
  }
  pub fn lookup_label(&self, label: &str) -> Option<&LabelKind> {
    self.label_scopes.iter().rev().filter_map(|hm| hm.get(label)).next()
  }

  pub fn register_name(
    &mut self, name: String, replacement: ValueExprKind,
  ) -> Option<ValueExprKind> {
    self.name_scopes.last_mut().unwrap().insert(name, replacement)
  }
  pub fn register_type(
    &mut self, name: String, replacement: TypeExprKind,
  ) -> Option<TypeExprKind> {
    self.type_scopes.last_mut().unwrap().insert(name, replacement)
  }
  pub fn register_label(
    &mut self, name: String, replacement: LabelKind,
  ) -> Option<LabelKind> {
    self.label_scopes.last_mut().unwrap().insert(name, replacement)
  }
}

pub fn resolve_names(ir: &mut IrNameResTypeCheck) {
  let mut ctx = ResolverContext::default();

  for module in ir.ast.modules.iter_mut() {
    ctx.within_scope(|ctx| {
      // scan all items as if they were defined simultaneously
      for item in module.items.iter() {
        let name = item.name.clone();
        let replacement = match &item.kind {
          ItemKind::Constant { .. } => ValueExprKind::NameOfConstant(item.id),
          ItemKind::StaticMmio { .. } => {
            ValueExprKind::NameOfStaticMmio(item.id)
          }
          ItemKind::Function { .. } => ValueExprKind::NameOfFunction(item.id),
          other => todo!("unknown how to register {other:?}"),
        };
        if ctx.register_name(name, replacement).is_some() {
          let name = item.name.as_str();
          ir.ast.errors.push(YagError {
            file_origin: item.file_origin,
            span: item.span,
            message: format!("Conflicting Definition: {name}"),
          });
        }
        // todo: register types too
      }

      for item in module.items.iter_mut() {
        resolve_inside_item(ctx, item);
      }
    });
  }
}

fn resolve_inside_item(ctx: &mut ResolverContext, item: &mut Item) {
  match &mut item.kind {
    ItemKind::StaticMmio { location, type_decl } => {
      resolve_inside_value_expr(ctx, location);
      resolve_inside_type_expr(ctx, type_decl);
    }
    ItemKind::Constant { type_decl, value_decl } => {
      resolve_inside_type_expr(ctx, type_decl);
      resolve_inside_value_expr(ctx, value_decl);
    }
    ItemKind::Function { args, ret_ty, statements } => {
      resolve_inside_type_expr(ctx, ret_ty);
      for arg in args.iter_mut() {
        resolve_inside_type_expr(ctx, &mut arg.type_decl);
      }
      ctx.within_scope(|ctx| {
        for arg in args.iter_mut() {
          match &mut arg.pattern.kind {
            PatternKind::Simple(name) => {
              let id = LocalNameId::new();
              let name = name.clone();
              let replacement = ValueExprKind::NameOfLocalVariable(id);
              let _ = ctx.register_name(name, replacement);
              arg.pattern.kind = PatternKind::SimpleLocalName(id);
            }
            other => todo!("unhandled pattern kind: {other:?}"),
          }
        }
        resolve_inside_body(ctx, statements);
      });
    }
    other => todo!("unhandled inside item: {other:?}"),
  }
}

fn resolve_inside_body(
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
          }
          items_defined_this_scope.push(item.name.as_str());
          let name = item.name.clone();
          let replacement = match &item.kind {
            ItemKind::Constant { .. } => ValueExprKind::NameOfConstant(item.id),
            ItemKind::StaticMmio { .. } => {
              ValueExprKind::NameOfStaticMmio(item.id)
            }
            ItemKind::Function { .. } => ValueExprKind::NameOfFunction(item.id),
            other => todo!("unknown how to register {other:?}"),
          };
          ctx.register_name(name, replacement);
        }
        _ => continue,
      }
    }
    for statement in statements.iter_mut() {
      resolve_inside_statement(ctx, statement);
    }
  });
}

fn resolve_inside_statement(
  ctx: &mut ResolverContext, statement: &mut Statement,
) {
  match &mut *statement.kind {
    StatementKind::Let { pattern, type_decl, initializer } => {
      if let Some(xpr) = initializer {
        resolve_inside_value_expr(ctx, xpr);
      }
      if let Some(ty) = type_decl {
        resolve_inside_type_expr(ctx, ty);
      }
      match &mut pattern.kind {
        PatternKind::Simple(name) => {
          let id = LocalNameId::new();
          let name = name.clone();
          let replacement = ValueExprKind::NameOfLocalVariable(id);
          let _ = ctx.register_name(name, replacement);
          pattern.kind = PatternKind::SimpleLocalName(id);
        }
        other => todo!("unhandled let pattern kind: {other:?}"),
      }
    }
    StatementKind::Expression(xpr) => resolve_inside_value_expr(ctx, xpr),
    StatementKind::Item(item) => resolve_inside_item(ctx, item),
    StatementKind::ErrStatementKind => return,
  }
}

fn resolve_inside_value_expr(ctx: &mut ResolverContext, xpr: &mut ValueExpr) {
  match &mut *xpr.kind {
    ValueExprKind::Identifier(name) => {
      if let Some(replacement) = ctx.lookup_name(name.as_str()) {
        *xpr.kind = replacement.clone();
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
              let replacement = LabelKind::GlobalId(id);
              let _ = ctx.register_label(name, replacement);
              label.kind = LabelKind::GlobalId(id);
            }
            other => todo!("unhandled let pattern kind: {other:?}"),
          }
        } else {
          let id = LabelId::new();
          let name = String::from("");
          let replacement = LabelKind::GlobalId(id);
          let _ = ctx.register_label(name, replacement);
          *label = Some(Label {
            span: Span::default(),
            kind: LabelKind::GlobalId(id),
          });
        }
        resolve_inside_body(ctx, statements);
      });
    }
    ValueExprKind::Break { label, value } => {
      match label {
        Some(label_inner) => match &mut label_inner.kind {
          LabelKind::Identifier(l) => {
            if let Some(replacement) = ctx.lookup_label(l.as_str()) {
              label_inner.kind = replacement.clone();
            }
          }
          _ => todo!("unhadnled label kind in break expr"),
        },
        None => {
          if let Some(replacement) = ctx.lookup_label("") {
            *label =
              Some(Label { span: Span::default(), kind: replacement.clone() });
          } else {
            // todo: error, break not within looping expression
          }
        }
      }
      if let Some(xpr) = value {
        resolve_inside_value_expr(ctx, xpr);
      }
    }
    ValueExprKind::If { condition, when_true, when_false } => {
      resolve_inside_value_expr(ctx, condition);
      resolve_inside_body(ctx, when_true);
      resolve_inside_body(ctx, when_false);
    }
    ValueExprKind::BinOp { left, op: _, right } => {
      resolve_inside_value_expr(ctx, left);
      resolve_inside_value_expr(ctx, right);
    }
    ValueExprKind::UnOp { op: _, operand } => {
      resolve_inside_value_expr(ctx, operand);
    }
    other => todo!("unhandled inside value expression: {other:?}"),
  }
}
fn resolve_inside_type_expr(_ctx: &mut ResolverContext, ty: &mut TypeExpr) {
  match &mut *ty.kind {
    TypeExprKind::Simple(_) => {
      // todo: get the type id for this type and overwrite the kind.
    }
    other => todo!("unhandled inside type expression: {other:?}"),
  }
}
