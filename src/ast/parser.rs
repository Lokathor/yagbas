#![allow(unused_imports)]

use std::{ffi::OsString, ops::Range};

use str_id::StrId;

use crate::{
  ast::{
    AstItem, AstModule, AstStaticMmio, AstTypeExpr, AstTypeExprKind,
    AstValExpr, AstValExprKind,
  },
  cst::{
    Cst, CstElem,
    CstKind::{self, InfixOperator, MmioLocation, TypeExpr, ValExpr},
  },
  tokenizer::{
    Token,
    TokenKind::{
      self, ClParen, Colon, Ident, KwMmio, KwStatic, LitNum, OpBracket,
      OpParen, Semicolon,
    },
  },
};

/// `let t = expect_tk_kind!(it, KIND);`
macro_rules! expect_tk_kind {
  ($it:expr, $kind:tt) => {
    if let Some(token) = $it.next().and_then(CstElem::token)
      && token.kind == $kind
    {
      token
    } else {
      return AstItem::ErrAstItem;
    }
  };
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
  ($it:expr, $kind:tt) => {{
    if let Some(tree) = $it.next().and_then(CstElem::tree)
      && tree.kind == $kind
    {
      tree
    } else {
      return AstItem::ErrAstItem;
    }
  }};
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
  pub filename: OsString,
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
  pub fn parse_module(&self, cst: &Cst) -> Option<AstModule> {
    if cst.kind != CstKind::Module {
      return None;
    }
    let mut module =
      AstModule { filename: self.filename.clone(), items: Vec::new() };

    for element in cst.iter_important() {
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
        _other => {
          dbg!(_other);
          module.items.push(AstItem::ErrAstItem)
        }
      }
    }

    Some(module)
  }

  pub fn parse_static_mmio(&self, cst: &Cst) -> AstItem {
    if cst.kind != CstKind::ItemStatic {
      return AstItem::ErrAstItem;
    }
    let mut out = AstStaticMmio::default();
    let mut it = cst.iter_important();
    out.span.start = expect_tk_kind!(it, KwStatic).span_within(&self.src).start;
    expect_tk_kind!(it, KwMmio);
    expect_tk_kind!(it, OpParen);
    out.location = self.parse_val_expr(expect_cst_kind!(it, ValExpr));
    expect_tk_kind!(it, ClParen);
    let (id, span) = self.token_id_span(expect_tk_kind!(it, Ident));
    out.name = id;
    out.name_span = span;
    expect_tk_kind!(it, Colon);
    out.ty = self.parse_type_expr(expect_cst_kind!(it, TypeExpr));
    out.span.end = expect_tk_kind!(it, Semicolon).span_within(&self.src).end;
    AstItem::StaticMmio(out)
  }
}
