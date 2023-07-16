use crate::{block_entry::BlockEntry, section_decl::SectionDecl};

use super::*;

#[derive(Clone, PartialEq, Eq)]
pub struct SectionFormed {
  pub name: (StaticStr, SimpleSpan),
  pub location: Vec<(TokenTree, SimpleSpan)>,
  pub block: Vec<(BlockEntry, SimpleSpan)>,
}

impl TryFrom<SectionDecl> for SectionFormed {
  type Error = Vec<CowStr>;
  fn try_from(decl: SectionDecl) -> Result<Self, Self::Error> {
    Ok(Self {
      name: decl.name,
      location: decl.location.clone(),
      block: run_parser(
        BlockEntry::parser().map_with_span(id2).repeated().collect::<Vec<_>>(),
        &decl.block,
      )
      .into_result()
      .map_err(|errs| {
        errs.into_iter().map(|e| Cow::Owned(format!("{e:?}"))).collect::<Vec<_>>()
      })?,
    })
  }
}

impl core::fmt::Debug for SectionFormed {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "SectionFormed {:?} [ ", self.name.0)?;
    for (i, (x, _)) in self.location.iter().enumerate() {
      if i > 0 {
        write!(f, " ")?;
      }
      write!(f, "{x:?}")?;
    }
    writeln!(f, " ] {{")?;
    for (be, _) in self.block.iter() {
      writeln!(f, "{be:?}")?;
    }
    write!(f, "}}")?;
    Ok(())
  }
}
