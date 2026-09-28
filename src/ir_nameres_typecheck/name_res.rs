use std::collections::HashMap;

use crate::{
  LabelId, LocalNameId,
  ast::{
    Item, ItemKind, Label, Module, Statement, StatementKind, TypeExpr,
    TypeExprKind, ValueExpr, ValueExprKind, visitor::TreeVisitMut,
  },
};

type ValueScope = HashMap<String, ValueExprKind>;
type TypeScope = HashMap<String, TypeExprKind>;
type LabelScope = (String, LabelId);
type StashedData = (Vec<ValueScope>, Vec<LabelScope>);

#[derive(Debug, Default)]
pub struct NameResolver {
  pub nonlocal_var_scopes: Vec<ValueScope>,
  pub local_var_scopes: Vec<ValueScope>,
  pub type_scopes: Vec<TypeScope>,
  pub label_scopes: Vec<LabelScope>,
  pub stash: Vec<StashedData>,
}
impl NameResolver {
  fn scope_add_vx(&mut self, name: String, replacement: ValueExprKind) {
    self.nonlocal_var_scopes.last_mut().unwrap().insert(name, replacement);
  }
  fn scope_add_tyx(&mut self, name: String, replacement: TypeExprKind) {
    self.type_scopes.last_mut().unwrap().insert(name, replacement);
  }
  fn register_items<'a>(&mut self, items: impl Iterator<Item = &'a Item>) {
    let mut names_registered_this_scope = Vec::new();
    for item in items {
      let name = item.name.as_str();
      if names_registered_this_scope.contains(&name) {
        todo!("multiple definitions");
      } else {
        names_registered_this_scope.push(name);
        match &item.kind {
          ItemKind::ErrItemKind => (),
          ItemKind::Constant(_) => {
            let replacement = ValueExprKind::NameOfConstant(item.id);
            self.scope_add_vx(name.to_string(), replacement);
          }
          ItemKind::StaticMmio(_) => {
            let replacement = ValueExprKind::NameOfStaticMmio(item.id);
            self.scope_add_vx(name.to_string(), replacement);
          }
          ItemKind::StaticRam(_) => {
            let replacement = ValueExprKind::NameOfStaticRam(item.id);
            self.scope_add_vx(name.to_string(), replacement);
          }
          ItemKind::StaticRom(_) => {
            let replacement = ValueExprKind::NameOfStaticRom(item.id);
            self.scope_add_vx(name.to_string(), replacement);
          }
          ItemKind::Function(_) => {
            let replacement = ValueExprKind::NameOfFunction(item.id);
            self.scope_add_vx(name.to_string(), replacement);
          }
          ItemKind::Struct(_) => {
            let replacement = TypeExprKind::NameOfStruct(item.id);
            self.scope_add_tyx(name.to_string(), replacement);
          }
          ItemKind::Bitbag(_) => {
            let replacement = TypeExprKind::NameOfStruct(item.id);
            self.scope_add_tyx(name.to_string(), replacement);
          }
          ItemKind::Enum(_) => {
            let replacement = TypeExprKind::NameOfStruct(item.id);
            self.scope_add_tyx(name.to_string(), replacement);
          }
          ItemKind::Impl(_) => (),
          ItemKind::Use(_) => {
            todo!("somehow this should put a thing into scope.")
          }
          ItemKind::Mod => (),
        }
      }
    }
  }
  fn lookup_var(&mut self, name: &str) -> Option<&ValueExprKind> {
    self
      .nonlocal_var_scopes
      .iter()
      .rev()
      .zip(self.local_var_scopes.iter().rev())
      .find_map(|(hm0, hm1)| {
        debug_assert!(!(hm0.contains_key(name) && hm1.contains_key(name)));
        hm0.get(name).or_else(|| hm1.get(name))
      })
  }
  fn lookup_type(&mut self, name: &str) -> Option<&TypeExprKind> {
    #[allow(clippy::unnecessary_lazy_evaluations)]
    self.type_scopes.iter().rev().find_map(|hm| hm.get(name)).or_else(|| {
      match name {
        "()" => Some(&TypeExprKind::Unit),
        "bool" => Some(&TypeExprKind::Bool),
        "u8" => Some(&TypeExprKind::U8),
        "i8" => Some(&TypeExprKind::I8),
        "u16" => Some(&TypeExprKind::U16),
        "i16" => Some(&TypeExprKind::I16),
        _ => None,
      }
    })
  }
  fn lookup_label(&mut self, name: &str) -> Option<LabelId> {
    self
      .label_scopes
      .iter()
      .rev()
      .find_map(|(n, i)| if name == n.as_str() { Some(*i) } else { None })
  }
}
impl TreeVisitMut for NameResolver {
  fn visit_module(&mut self, module: &mut Module) {
    self.register_items(module.items.iter());
  }

  fn visit_statement_vec(&mut self, statements: &mut Vec<Statement>) {
    self.register_items(statements.iter().filter_map(|statement| {
      if let StatementKind::Item(item) = &*statement.kind {
        Some(item)
      } else {
        None
      }
    }));
  }

  fn push_label_point(&mut self, label: &mut Label) {
    let name = label.name.to_string();
    let id = match label.opt_id {
      Some(id) => id,
      None => {
        let new_id = LabelId::new();
        label.opt_id = Some(new_id);
        new_id
      }
    };
    self.label_scopes.push((name, id));
  }
  fn pop_label_point(&mut self) {
    self.label_scopes.pop();
  }

  fn push_block_point(&mut self) {
    self.nonlocal_var_scopes.push(HashMap::default());
    self.type_scopes.push(HashMap::default());
    self.local_var_scopes.push(HashMap::default());
  }
  fn pop_block_point(&mut self) {
    self.nonlocal_var_scopes.pop();
    self.type_scopes.pop();
    self.local_var_scopes.pop();
  }
  fn register_block_local(&mut self, vx: &mut ValueExpr) {
    match &mut *vx.kind {
      ValueExprKind::Identifier(name) => {
        let name = name.to_string();
        let local_id = LocalNameId::new();
        let replacement = ValueExprKind::NameOfLocalVariable(local_id);
        self.local_var_scopes.last_mut().unwrap().insert(name, replacement);
      }
      other => {
        todo!("block local declaration, expected Identifier, got: {other:?}")
      }
    }
  }

  fn stash_locals_and_labels(&mut self) {
    let old_local_vars = core::mem::take(&mut self.local_var_scopes);
    let old_labels = core::mem::take(&mut self.label_scopes);
    self.stash.push((old_local_vars, old_labels));
  }
  fn unstash_locals_and_labels(&mut self) {
    debug_assert!(self.local_var_scopes.is_empty());
    debug_assert!(self.label_scopes.is_empty());
    let (old_local_vars, old_labels) = self.stash.pop().unwrap();
    self.local_var_scopes = old_local_vars;
    self.label_scopes = old_labels;
  }

  fn visit_value_expr(&mut self, vx: &mut ValueExpr) {
    if let ValueExprKind::Identifier(name) = &mut *vx.kind
      && let Some(replacement) = self.lookup_var(name)
    {
      *vx.kind = replacement.clone();
    }
  }

  fn visit_type_expr(&mut self, tyx: &mut TypeExpr) {
    if let TypeExprKind::Identifier(name) = &mut *tyx.kind
      && let Some(replacement) = self.lookup_type(name)
    {
      *tyx.kind = replacement.clone();
    }
  }

  fn visit_label_expr(&mut self, label: &mut Label) {
    if label.opt_id.is_none() {
      label.opt_id = self.lookup_label(&label.name);
    }
  }
}
