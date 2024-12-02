use super::*;

#[derive(Debug, Clone)]
pub enum Item {
  Function(FileSpanned<Function>),
  ItemError,
}
