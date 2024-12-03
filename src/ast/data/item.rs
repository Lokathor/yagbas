use super::*;

#[derive(Debug, Clone)]
pub enum Item {
  Const(FileSpanned<Const>),
  Function(FileSpanned<Function>),
  Static(FileSpanned<Static>),
  ItemError,
}
impl Item {
  pub fn get_name(&self) -> Option<StrID> {
    match self {
      Item::Const(fs_const) => Some(fs_const.name._payload),
      Item::Function(fs_function) => Some(fs_function.name._payload),
      Item::Static(fs_static) => Some(fs_static.name._payload),
      Item::ItemError => None,
    }
  }
}
