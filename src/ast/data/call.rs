use super::*;

#[derive(Debug, Clone)]
pub struct Call {
  pub target: FileSpanned<StrID>,
  pub args: Vec<FileSpanned<TokenTree>>,
}
