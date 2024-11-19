use std::sync::atomic::{AtomicUsize, Ordering};

use token_tree::TokenTree;

use crate::{
  src_files::{FileSpan, FileSpanned},
  str_id::StrID,
};

pub mod parsing;
pub mod token;
pub mod token_tree;

#[derive(Debug, Clone)]
pub enum Item {
  Function(Function),
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
    }
  }
}

#[derive(Debug, Clone, Copy)]
pub enum ItemKind {
  Function,
  Const,
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

/// A "single chunk of code" in yagbas.
///
/// Most statements are "one line" of code, but the variants for control flow
/// have a body of inner statements that they cover.
#[derive(Debug, Clone)]
pub enum Statement {
  Return,
  Call { target: StrID, args: Vec<FileSpanned<TokenTree>> },
  Loop(Loop),
  Continue(StrID),
  Break(StrID),
  AssignReg8Const { target: Reg8, value: StrID },
  StatementError,
}
impl Statement {
  #[inline]
  #[must_use]
  pub fn targets_called(&self) -> Vec<StrID> {
    match self {
      Statement::Call { target, .. } => vec![*target],
      Statement::Loop(Loop { statements, .. }) => {
        statements.iter().flat_map(|s| s.targets_called()).collect()
      }
      _ => Vec::new(),
    }
  }
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
