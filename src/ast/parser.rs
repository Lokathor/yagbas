use std::ffi::OsString;

use str_id::StrId;

use crate::{
  ast::{AstItem, AstModule, AstStaticMmio, AstTypeExpr, AstValExpr},
  cst::{Cst, CstElem, CstKind},
  tokenizer::{
    Token,
    TokenKind::{ClParen, Colon, KwMmio, KwStatic, OpParen, Semicolon},
  },
};

#[derive(Debug, Clone)]
pub struct AstParser {
  pub filename: OsString,
  pub src: String,
}
impl AstParser {
  pub fn parse_module(&self, cst: &Cst) -> Option<AstModule> {
    if cst.kind != CstKind::Module {
      return None;
    }
    let mut module =
      AstModule { filename: self.filename.clone(), items: Vec::new() };

    for element in &cst.elements {
      match element {
        CstElem::Tree(sub_cst) => match sub_cst.kind {
          CstKind::ItemStatic => {
            let i = if sub_cst.tokens_here().any(|tk| tk.kind == KwMmio) {
              self.parse_static_mmio(sub_cst)
            } else {
              AstItem::ErrAstItem
            };
            module.items.push(i);
          }
          CstKind::ItemConst => continue,
          CstKind::ItemFunction => continue,
          _ => module.items.push(AstItem::ErrAstItem),
        },
        _ => module.items.push(AstItem::ErrAstItem),
      }
    }

    Some(module)
  }

  pub fn parse_static_mmio(&self, cst: &Cst) -> AstItem {
    if cst.kind != CstKind::ItemStatic {
      return AstItem::ErrAstItem;
    }
    println!("{cst}");
    let mut out = AstStaticMmio {
      location: AstValExpr::ErrAstValExpr,
      location_span: 0..0,
      name: StrId::default(),
      name_span: 0..0,
      ty: AstTypeExpr::ErrAstTypeExpr,
      ty_span: 0..0,
    };
    let mut it = cst.iter_important();
    if !matches!(it.next(), Some(CstElem::Token(Token { kind: KwStatic, .. })))
    {
      return AstItem::ErrAstItem;
    }
    if !matches!(it.next(), Some(CstElem::Token(Token { kind: KwMmio, .. }))) {
      return AstItem::ErrAstItem;
    }
    if !matches!(it.next(), Some(CstElem::Token(Token { kind: OpParen, .. }))) {
      return AstItem::ErrAstItem;
    }
    let _location_expr = it.next(); // TODO
    if !matches!(it.next(), Some(CstElem::Token(Token { kind: ClParen, .. }))) {
      return AstItem::ErrAstItem;
    }
    let _name = it.next(); // TODO
    if !matches!(it.next(), Some(CstElem::Token(Token { kind: Colon, .. }))) {
      return AstItem::ErrAstItem;
    }
    let _type_expr = it.next(); // TODO
    if !matches!(it.next(), Some(CstElem::Token(Token { kind: Semicolon, .. })))
    {
      return AstItem::ErrAstItem;
    }

    AstItem::StaticMmio(out)
  }
}
