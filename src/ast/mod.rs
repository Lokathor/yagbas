use std::sync::atomic::{AtomicUsize, Ordering};

use const_expr::ConstExpr;
use token_tree::TokenTree;

use crate::{
  src_files::{FileSpan, FileSpanned},
  str_id::StrID,
};

pub mod const_expr;
pub mod parsing;
pub mod token;
pub mod token_tree;

mod data;
pub use data::*;

#[derive(Debug, Clone)]
pub enum Item {
  Function(Function),
  NamedConst(NamedConst),
  ItemError,
}
impl Item {
  /// The name of the item, if any.
  ///
  /// Currently, only errors do not have a name.
  #[inline]
  #[must_use]
  pub fn get_name(&self) -> Option<FileSpanned<StrID>> {
    match self {
      Item::Function(Function { name, .. }) => Some(*name),
      Item::ItemError => None,
      Item::NamedConst(NamedConst { name, .. }) => Some(*name),
    }
  }

  /// Gets the `Function` contained, if any.
  #[inline]
  #[must_use]
  pub fn get_function(&self) -> Option<&Function> {
    match self {
      Self::Function(f) => Some(f),
      _ => None,
    }
  }

  #[inline]
  #[must_use]
  pub fn kind(&self) -> ItemKind {
    match self {
      Item::Function(function) => ItemKind::Function,
      Item::ItemError => ItemKind::Error,
      Item::NamedConst(named_const) => ItemKind::NamedConst,
    }
  }
}

#[derive(Debug, Clone, Copy)]
pub enum ItemKind {
  Function,
  NamedConst,
  Static,
  Error,
}
impl core::fmt::Display for ItemKind {
  #[inline]
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    core::fmt::Debug::fmt(self, f)
  }
}

#[derive(Debug, Clone)]
pub struct Function {
  pub name: FileSpanned<StrID>,
  pub arguments: Vec<FileSpanned<TokenTree>>,
  pub statements: Vec<FileSpanned<Statement>>,
}
impl Function {
  #[inline]
  #[must_use]
  pub fn get_name(&self) -> StrID {
    self.name._payload
  }

  #[inline]
  #[must_use]
  pub fn targets_called(&self) -> Vec<FileSpanned<StrID>> {
    self
      .statements
      .iter()
      .flat_map(|s| {
        s.targets_called()
          .into_iter()
          .map(|_payload| FileSpanned { _payload, _span: s._span })
      })
      .collect()
  }
}

/// A named constant expression.
#[derive(Debug, Clone)]
pub struct NamedConst {
  pub name: FileSpanned<StrID>,
  pub expr: FileSpanned<ConstExpr>,
}

#[derive(Debug, Clone, Copy)]
pub enum Reg8 {
  A,
  B,
  C,
  D,
  E,
  H,
  L,
}

#[derive(Debug, Clone)]
pub struct Loop {
  pub name: StrID,
  pub id: usize,
  pub statements: Vec<FileSpanned<Statement>>,
}
impl Loop {
  /// Gets the next ID value for a new loop.
  ///
  /// This is automatically used by `new` and `new_with_name`.
  #[inline]
  pub fn get_next_id() -> usize {
    static NEXT_LOOP_ID: AtomicUsize = AtomicUsize::new(1);
    NEXT_LOOP_ID.fetch_add(1, Ordering::Relaxed)
  }

  #[inline]
  pub fn new(statements: Vec<FileSpanned<Statement>>) -> Self {
    Self::new_with_name(StrID::default(), statements)
  }
  #[inline]
  pub fn new_with_name(
    name: StrID, statements: Vec<FileSpanned<Statement>>,
  ) -> Self {
    Self { name, statements, id: Self::get_next_id() }
  }

  #[inline]
  pub fn make_start_name(&self) -> String {
    let x = self.id;
    let name = self.name;
    format!(".loop{x}#{name}#start")
  }
  #[inline]
  pub fn make_end_name(&self) -> String {
    let x = self.id;
    let name = self.name;
    format!(".loop{x}#{name}#end")
  }
}
