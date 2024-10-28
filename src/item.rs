use crate::{
  src_files::FileSpanned, statement::Statement, str_id::StrID,
  token_tree::TokenTree,
};

#[derive(Debug, Clone)]
pub enum Item {
  Function(Function),
  ItemError,
}

#[derive(Debug, Clone)]
pub struct Function {
  pub name: FileSpanned<StrID>,
  pub arguments: Vec<FileSpanned<TokenTree>>,
  pub statements: Vec<FileSpanned<TokenTree>>,
}
