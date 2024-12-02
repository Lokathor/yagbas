use super::*;

#[derive(Debug, Clone)]
pub enum Item {
  Function(FileSpanned<Function>),
  Const(FileSpanned<Const>),
  Static(FileSpanned<Static>),
  ItemError,
}
