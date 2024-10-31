use crate::{
  src_files::FileSpanned, statement::Statement, str_id::StrID,
  token_tree::TokenTree,
};

#[derive(Debug, Clone)]
pub enum Item {
  Function(Function),
  ItemError,
}
impl Item {
  /// The name of the item, but item errors do not have a name.
  pub fn get_name(&self) -> Option<FileSpanned<StrID>> {
    match self {
      Item::Function(Function { name, .. }) => Some(*name),
      Item::ItemError => None,
    }
  }
}

#[derive(Debug, Clone)]
pub struct Function {
  pub name: FileSpanned<StrID>,
  pub arguments: Vec<FileSpanned<TokenTree>>,
  pub statements: Vec<FileSpanned<Statement>>,
}
