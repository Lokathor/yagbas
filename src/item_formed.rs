use crate::{
  const_formed::ConstFormed, item_decl::ItemDecl, section_formed::SectionFormed,
};

use super::*;

#[derive(Clone)]
pub enum ItemFormed {
  Const(ConstFormed),
  Section(SectionFormed),
}
impl TryFrom<ItemDecl> for ItemFormed {
  type Error = Vec<CowStr>;
  fn try_from(decl: ItemDecl) -> Result<Self, Self::Error> {
    Ok(match decl {
      ItemDecl::ConstDecl(x) => Self::Const(x.try_into()?),
      ItemDecl::SectionDecl(x) => Self::Section(x.try_into()?),
    })
  }
}
impl core::fmt::Debug for ItemFormed {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      ItemFormed::Const(x) => write!(f, "{x:?}"),
      ItemFormed::Section(x) => write!(f, "{x:?}"),
    }
  }
}
