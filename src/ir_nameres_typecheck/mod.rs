use fnv::FnvHashMap;

use crate::{
  YagError,
  ast::{
    Ast, FunctionArg, Item, ItemKind, Pattern, Statement, StatementKind,
    TypeExpr, TypeExprKind, ValueExpr,
    ValueExprKind::{self},
  },
};

#[derive(Debug, Clone)]
pub struct IrNameResTypeCheck {
  pub ast: Ast,
}

type NameScopes = Vec<FnvHashMap<String, ValueExprKind>>;

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
      resolve_names_within_value_expr(name_scopes, &mut 0, value_decl);
    }
    ItemKind::StaticMmio { location, type_decl } => {
      resolve_names_within_value_expr(name_scopes, &mut 0, location);
      resolve_names_within_type_expr(name_scopes, type_decl);
    }
    ItemKind::Function { args, ret_ty, statements } => {
      for FunctionArg { type_decl, .. } in args.iter_mut() {
        resolve_names_within_type_expr(name_scopes, type_decl);
      }
      resolve_names_within_type_expr(name_scopes, ret_ty);
      //
      let mut local_count = 0;
      name_scopes.push(FnvHashMap::default());
      for FunctionArg { pattern, .. } in args.iter_mut() {
        resolve_names_within_pattern(name_scopes, &mut local_count, pattern);
      }
      for statement in statements.iter_mut() {
        resolve_names_within_statement(
          name_scopes,
          &mut local_count,
          statement,
        );
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

fn resolve_names_within_value_expr(
  name_scopes: &mut NameScopes, local_count: &mut u32, value: &mut ValueExpr,
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
        resolve_names_within_statement(name_scopes, local_count, statement);
      }
    }
    ValueExprKind::Break { label: _, value } => {
      // TODO: label resolution
      if let Some(x) = value {
        resolve_names_within_value_expr(name_scopes, local_count, x);
      }
    }
    ValueExprKind::If { condition, when_true, when_false } => {
      resolve_names_within_value_expr(name_scopes, local_count, condition);
      for statement in when_true.iter_mut() {
        resolve_names_within_statement(name_scopes, local_count, statement);
      }
      for statement in when_false.iter_mut() {
        resolve_names_within_statement(name_scopes, local_count, statement);
      }
    }
    ValueExprKind::BinOp { left, op: _, right } => {
      resolve_names_within_value_expr(name_scopes, local_count, left);
      resolve_names_within_value_expr(name_scopes, local_count, right);
    }
    ValueExprKind::UnOp { op: _, operand } => {
      resolve_names_within_value_expr(name_scopes, local_count, operand);
    }
    other => todo!("unhandled value expr kind: {other:?}"),
  }
}

fn resolve_names_within_pattern(
  _name_scopes: &mut NameScopes, _local_count: &mut u32, _pattern: &mut Pattern,
) {
  todo!("pattern")
}

fn resolve_names_within_statement(
  name_scopes: &mut NameScopes, local_count: &mut u32,
  statement: &mut Statement,
) {
  match &mut *statement.kind {
    StatementKind::Expression(value) => {
      resolve_names_within_value_expr(name_scopes, local_count, value);
    }
    other => todo!("unhandled statement kind: {other:?}"),
  }
}
