#![allow(unused_imports)]

use std::{ffi::OsString, ops::Range};

use str_id::{PathId, StrId};

use crate::{
  ast::{
    AstConstant, AstItem,
    AstItemKind::{self, ErrAstItemKind},
    AstModule, AstStaticMmio, AstTypeExpr, AstTypeExprKind, AstValExpr,
    AstValExprKind,
  },
  cst::{
    Cst, CstElem,
    CstKind::{self, InfixOperator, MmioLocation, TypeExpr, ValExpr},
  },
  tokenizer::{
    Token,
    TokenKind::{
      self, ClParen, Colon, Equal, Ident, KwConst, KwMmio, KwStatic, LitNum,
      OpBracket, OpParen, Semicolon,
    },
  },
};

/// `let t = expect_tk_kind!(it, KIND);`
macro_rules! expect_tk_kind {
  ($it:expr, $kind:tt, $out:expr) => {
    if let Some(token) = $it.next().and_then(CstElem::token)
      && token.kind == $kind
    {
      token
    } else {
      return $out;
    }
  };
}
/// `let cst = expect_cst_kind!(it, KIND);`
macro_rules! expect_cst_kind {
  ($it:expr, $kind:tt, $out:expr) => {{
    if let Some(tree) = $it.next().and_then(CstElem::tree)
      && tree.kind == $kind
    {
      tree
    } else {
      return $out;
    }
  }};
}

#[derive(Debug, Clone)]
pub struct AstParser {
  pub path_id: PathId,
  pub src: String,
}
type InfixMaker = fn(Box<AstValExpr>, Box<AstValExpr>) -> AstValExprKind;

impl AstParser {
  pub fn token_id_span(&self, tk: Token) -> (StrId, Range<usize>) {
    let span = tk.span_within(&self.src);
    let src_str = &self.src[span.clone()];
    let id = StrId::from(src_str);
    (id, span)
  }
  pub fn parse_val_expr(&self, cst: &Cst) -> AstValExpr {
    let mut out = AstValExpr::default();
    if cst.kind != CstKind::ValExpr {
      out.span = cst.span_within(&self.src);
      return out;
    }
    let mut it = cst.iter_important().peekable();
    match it.next() {
      Some(CstElem::Token(t)) if t.kind == LitNum => {
        let (id, span) = self.token_id_span(*t);
        out.kind = AstValExprKind::LiteralNumber(id);
        out.span = span;
        debug_assert!(it.peek().is_none());
        return out;
      }
      Some(CstElem::Token(t)) if t.kind == Ident => {
        let (id, span) = self.token_id_span(*t);
        out.kind = AstValExprKind::Identifier(id);
        out.span = span;
        debug_assert!(it.peek().is_none());
        return out;
      }
      Some(CstElem::Tree(cst)) if cst.kind == ValExpr => {
        let lhs = self.parse_val_expr(cst);
        let op_cst = expect_cst_kind!(it, InfixOperator, out);
        let Some(infix) = self.parse_infix_operator(op_cst) else {
          return out;
        };
        let rhs_cst = expect_cst_kind!(it, ValExpr, out);
        let rhs = self.parse_val_expr(rhs_cst);
        out.span = lhs.span.start..rhs.span.end;
        out.kind = infix(Box::new(lhs), Box::new(rhs));
        return out;
      }
      _other => {
        out.span = cst.span_within(&self.src);
        return out;
      }
    }
  }
  pub fn parse_infix_operator(&self, cst: &Cst) -> Option<InfixMaker> {
    let mut op_iter = cst.iter_important();
    if op_iter.next().and_then(CstElem::token).map(|t| t.kind)
      == Some(TokenKind::Star)
      && op_iter.next().is_none()
    {
      Some(AstValExprKind::Multiply)
    } else {
      todo!()
    }
  }
  pub fn parse_type_expr(&self, cst: &Cst) -> AstTypeExpr {
    let mut out = AstTypeExpr::default();
    out.span = cst.span_within(&self.src);
    if cst.kind != CstKind::TypeExpr {
      return out;
    }
    let mut it = cst.iter_important().peekable();
    match it.next() {
      Some(CstElem::Token(t)) if t.kind == Ident => {
        let (id, _) = self.token_id_span(*t);
        out.kind = AstTypeExprKind::Plain(id);
        if it.peek().is_none() {
          return out;
        } else {
          todo!()
        }
      }
      Some(CstElem::Token(t)) if t.kind == OpBracket => {
        let elem_cst = expect_cst_kind!(it, TypeExpr, out);
        expect_tk_kind!(it, Semicolon, out);
        let elem_ty = self.parse_type_expr(elem_cst);
        let len_cst = expect_cst_kind!(it, ValExpr, out);
        let length = self.parse_val_expr(len_cst);
        out.kind = AstTypeExprKind::Array {
          element_ty: Box::new(elem_ty),
          length: Box::new(length),
        };
        return out;
      }
      _ => return out,
    }
  }
  pub fn parse_module(&self, cst: &Cst) -> AstModule {
    debug_assert_eq!(cst.kind, CstKind::Module);
    let mut out = AstModule { path_id: self.path_id, items: Vec::new() };

    for element in cst.iter_important() {
      let mut item = AstItem::default();
      match element {
        CstElem::Tree(tree) => {
          item.span = tree.span_within(&self.src);
          match tree.kind {
            CstKind::ItemStatic => {
              item.kind = self.parse_static_mmio(tree);
            }
            CstKind::ItemConst => {
              item.kind = self.parse_constant(tree);
            }
            CstKind::ItemFunction => {
              dbg!("todo, function parsing");
            }
            _other_cst_kind => {
              dbg!(_other_cst_kind);
            }
          }
        }
        CstElem::Token(token) => {
          item.span = token.span_within(&self.src);
          dbg!(token);
        }
      }
      out.items.push(item);
    }

    out
  }

  pub fn parse_static_mmio(&self, cst: &Cst) -> AstItemKind {
    debug_assert_eq!(cst.kind, CstKind::ItemStatic);
    let mut out = AstStaticMmio::default();
    let mut it = cst.iter_important();
    expect_tk_kind!(it, KwStatic, ErrAstItemKind);
    expect_tk_kind!(it, KwMmio, ErrAstItemKind);
    expect_tk_kind!(it, OpParen, ErrAstItemKind);
    out.address =
      self.parse_val_expr(expect_cst_kind!(it, ValExpr, ErrAstItemKind));
    expect_tk_kind!(it, ClParen, ErrAstItemKind);
    let (id, span) =
      self.token_id_span(expect_tk_kind!(it, Ident, ErrAstItemKind));
    out.name = id;
    out.name_span = span;
    expect_tk_kind!(it, Colon, ErrAstItemKind);
    out.ty =
      self.parse_type_expr(expect_cst_kind!(it, TypeExpr, ErrAstItemKind));
    expect_tk_kind!(it, Semicolon, ErrAstItemKind);
    AstItemKind::StaticMmio(out)
  }

  fn parse_constant(&self, cst: &Cst) -> AstItemKind {
    debug_assert_eq!(cst.kind, CstKind::ItemConst);
    let mut out = AstConstant::default();
    let mut it = cst.iter_important();
    expect_tk_kind!(it, KwConst, ErrAstItemKind);
    let (id, span) =
      self.token_id_span(expect_tk_kind!(it, Ident, ErrAstItemKind));
    out.name = id;
    out.name_span = span;
    expect_tk_kind!(it, Colon, ErrAstItemKind);
    out.ty =
      self.parse_type_expr(expect_cst_kind!(it, TypeExpr, ErrAstItemKind));
    expect_tk_kind!(it, Equal, ErrAstItemKind);
    out.xpr =
      self.parse_val_expr(expect_cst_kind!(it, ValExpr, ErrAstItemKind));
    expect_tk_kind!(it, Semicolon, ErrAstItemKind);
    AstItemKind::Constant(out)
  }
}
