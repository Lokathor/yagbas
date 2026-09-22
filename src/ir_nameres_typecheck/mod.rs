use fnv::FnvHashMap;

use crate::{
  YagError,
  ast::{
    Ast, FunctionArg, Item, ItemKind, LabelKind, Pattern, Statement,
    StatementKind, TypeExpr, TypeExprKind, ValueExpr,
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
  let mut name_scopes = Vec::new();
  // there are currently no names that are in scope by default, but maybe there
  // should be.

  for module in ir.ast.modules.iter_mut() {
    name_scopes.push(FnvHashMap::default());

    let this_scope = name_scopes.last_mut().unwrap();

    // Scan all items at this scope and gather newly defined names before
    // digging inside of anything.
    for item in module.items.iter() {
      let name = item.name.clone();
      let replacement = match &item.kind {
        ItemKind::Constant { .. } => ValueExprKind::NameOfConstant(item.id),
        ItemKind::StaticMmio { .. } => ValueExprKind::NameOfStaticMmio(item.id),
        ItemKind::Function { .. } => ValueExprKind::NameOfFunction(item.id),
        _other => todo!("{_other:?}"),
      };
      if this_scope.insert(name, replacement).is_some() {
        let name = &item.name;
        ir.ast.errors.push(YagError {
          file_origin: item.file_origin,
          span: item.span,
          message: format!("Conflicting Definition: {name}"),
        });
      }
    }

    for item in module.items.iter_mut() {
      resolve_names_within_item(&mut name_scopes, item);
    }

    name_scopes.pop();
  }
}

fn resolve_names_within_item(name_scopes: &mut NameScopes, item: &mut Item) {
  match &mut item.kind {
    ItemKind::Constant { type_decl, value_decl } => {
      resolve_names_within_type_expr(name_scopes, type_decl);
      resolve_names_within_value_expr(name_scopes, value_decl);
    }
    ItemKind::StaticMmio { location, type_decl } => {
      resolve_names_within_value_expr(name_scopes, location);
      resolve_names_within_type_expr(name_scopes, type_decl);
    }
    ItemKind::Function { args, ret_ty, statements } => {
      for FunctionArg { type_decl, .. } in args.iter_mut() {
        resolve_names_within_type_expr(name_scopes, type_decl);
      }
      resolve_names_within_type_expr(name_scopes, ret_ty);
      //
      name_scopes.push(FnvHashMap::default());
      for FunctionArg { pattern, .. } in args.iter_mut() {
        resolve_names_within_pattern(name_scopes, pattern);
      }
      for statement in statements.iter_mut() {
        resolve_names_within_statement(name_scopes, statement);
      }
      name_scopes.pop();
    }
    _other => todo!("{_other:?}"),
  }
}

fn resolve_names_within_type_expr(
  _name_scopes: &mut NameScopes, ty: &mut TypeExpr,
) {
  match &mut *ty.kind {
    TypeExprKind::Simple(_) => return,
    other => todo!("unhandled type expr kind: {other:?}"),
  }
}

// todo: when handling for items/expressions with  body we need to scan for item names.

fn resolve_names_within_value_expr(
  name_scopes: &mut NameScopes, value: &mut ValueExpr,
) {
  match &mut *value.kind {
    ValueExprKind::LiteralNumber(_) => return,
    ValueExprKind::Identifier(x) => {
      if let Some(replacement) =
        name_scopes.iter().rev().filter_map(|hm| hm.get(x.as_str())).next()
      {
        *value.kind = replacement.clone();
      }
    }
    ValueExprKind::Loop { label: _, statements } => {
      // TODO: label resolution
      for statement in statements.iter_mut() {
        resolve_names_within_statement(name_scopes, statement);
      }
    }
    ValueExprKind::Break { label: _, value } => {
      // TODO: label resolution
      if let Some(x) = value {
        resolve_names_within_value_expr(name_scopes, x);
      }
    }
    ValueExprKind::If { condition, when_true, when_false } => {
      resolve_names_within_value_expr(name_scopes, condition);
      for statement in when_true.iter_mut() {
        resolve_names_within_statement(name_scopes, statement);
      }
      for statement in when_false.iter_mut() {
        resolve_names_within_statement(name_scopes, statement);
      }
    }
    ValueExprKind::BinOp { left, op: _, right } => {
      resolve_names_within_value_expr(name_scopes, left);
      resolve_names_within_value_expr(name_scopes, right);
    }
    ValueExprKind::UnOp { op: _, operand } => {
      resolve_names_within_value_expr(name_scopes, operand);
    }
    other => todo!("unhandled value expr kind: {other:?}"),
  }
}

fn resolve_names_within_pattern(
  _name_scopes: &mut NameScopes, _pattern: &mut Pattern,
) {
  todo!("pattern")
}

fn resolve_names_within_statement(
  name_scopes: &mut NameScopes, statement: &mut Statement,
) {
  match &mut *statement.kind {
    StatementKind::Expression(value) => {
      resolve_names_within_value_expr(name_scopes, value);
    }
    other => todo!("unhandled statement kind: {other:?}"),
  }
}
