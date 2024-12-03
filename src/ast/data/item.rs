use super::*;

#[derive(Debug, Clone)]
pub enum Item {
  Function(FileSpanned<Function>),
  Const(FileSpanned<Const>),
  Static(FileSpanned<Static>),
  ItemError,
}
impl Item {
  pub fn get_name(&self) -> Option<StrID> {
    match self {
      Item::Function(fs_function) => Some(fs_function.name._payload),
      Item::Const(fs_const) => Some(fs_const.name._payload),
      Item::Static(fs_static) => Some(fs_static.name._payload),
      Item::ItemError => None,
    }
  }
}
