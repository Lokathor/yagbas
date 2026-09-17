use crate::{
  ast::{AstModule, parser::AstParser},
  cst::{Cst, CstKind},
};

pub fn read_module(p: &mut AstParser, cst: &Cst) -> AstModule {
  debug_assert_eq!(cst.kind, CstKind::Module);
  //
  let mut out = AstModule::default();
  out.file_origin = p.file_origin;

  // TODO

  out
}
