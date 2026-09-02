#![allow(unused_imports)]

use std::{ffi::OsString, ops::Range};

use str_id::{PathId, StrId};
use tinyvec::ArrayVec;

use crate::{
  ast::{
    AstBody, AstConstant, AstExprType, AstExprTypeKind, AstExprVal,
    AstExprValKind::{self, ArrayIndex},
    AstFunction, AstFunctionArgument, AstItem,
    AstItemKind::{self, ErrAstItemKind},
    AstLet, AstModule, AstStatement,
    AstStatementKind::{self, ErrAstStatementKind},
    AstStaticMmio,
  },
  cst::{
    Cst, CstElem,
    CstKind::{
      self, ArgumentList, Body, ExprType, ExprVal, MmioLocation, OperatorInfix,
      OperatorPostfix, OperatorPrefix, ReturnType,
    },
    operators::{InfixOperator, PostfixOperator, PrefixOperator},
  },
  tokenizer::{
    Token,
    TokenKind::{
      self, ClBrace, ClBracket, ClParen, Colon, Equal, Ident, KwConst, KwFn,
      KwFor, KwIf, KwIn, KwLet, KwLoop, KwMmio, KwStatic, LitNum, OpBrace,
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
  ($it:expr, $kind:pat_param, $out:expr) => {{
    if let Some(tree) = $it.next().and_then(CstElem::tree)
      && match tree.kind {
        $kind => true,
        _ => false,
      }
    {
      tree
    } else {
      return $out;
    }
  }};
}

#[derive(Debug, Clone)]
pub struct AstParser {
  pub src: String,
}
type InfixMaker = fn(Box<AstExprVal>, Box<AstExprVal>) -> AstExprValKind;

impl AstParser {
  pub fn token_id_span(&self, tk: Token) -> (StrId, Range<usize>) {
    let span = tk.span_within(&self.src);
    let src_str = &self.src[span.clone()];
    let id = StrId::from(src_str);
    (id, span)
  }
  pub fn parse_expr_val(&self, cst: &Cst) -> AstExprVal {
    let mut out = AstExprVal::default();
    out.span = cst.span_within(&self.src);
    if ![CstKind::ExprVal, CstKind::ExprForVar, CstKind::ExprForRange]
      .contains(&cst.kind)
    {
      dbg!(cst.kind);
    }
    let mut it = cst.iter_important().peekable();
    match it.next() {
      Some(CstElem::Token(t)) if t.kind == LitNum => {
        let (id, span) = self.token_id_span(*t);
        out.kind = AstExprValKind::LiteralNumber(id);
        out.span = span;
        debug_assert!(it.peek().is_none());
        return out;
      }
      Some(CstElem::Token(t)) if t.kind == Ident => {
        let (id, span) = self.token_id_span(*t);
        out.kind = AstExprValKind::Identifier(id);
        out.span = span;
        debug_assert!(it.peek().is_none());
        return out;
      }
      Some(CstElem::Token(t)) if t.kind == KwLoop => {
        if let Some(CstElem::Tree(cst)) = it.next() {
          let body = self.parse_body(cst);
          out.kind = AstExprValKind::Loop(Box::new(body));
        }
        debug_assert!(it.peek().is_none());
        return out;
      }
      Some(CstElem::Token(t)) if t.kind == KwIf => {
        let condition = if let Some(CstElem::Tree(cst)) = it.next() {
          self.parse_expr_val(cst)
        } else {
          return out;
        };
        let body = if let Some(CstElem::Tree(cst)) = it.next() {
          self.parse_body(cst)
        } else {
          return out;
        };
        out.kind = AstExprValKind::If(Box::new(condition), Box::new(body));
        debug_assert!(it.peek().is_none());
        return out;
      }
      Some(CstElem::Token(t)) if t.kind == KwFor => {
        let step_expr = if let Some(CstElem::Tree(cst)) = it.next() {
          self.parse_expr_val(cst)
        } else {
          return out;
        };
        expect_tk_kind!(it, KwIn, out);
        let range_expr = if let Some(CstElem::Tree(cst)) = it.next() {
          self.parse_expr_val(cst)
        } else {
          return out;
        };
        let body = if let Some(CstElem::Tree(cst)) = it.next() {
          self.parse_body(cst)
        } else {
          return out;
        };
        out.kind = AstExprValKind::For(
          Box::new(step_expr),
          Box::new(range_expr),
          Box::new(body),
        );
        debug_assert!(it.peek().is_none());
        return out;
      }
      Some(CstElem::Tree(cst)) if cst.kind == ExprVal => {
        let lhs = self.parse_expr_val(cst);
        match it.next() {
          Some(CstElem::Tree(cst)) if matches!(cst.kind, OperatorInfix(_)) => {
            let infix = self.parse_infix_operator(cst).unwrap();
            let rhs_cst = expect_cst_kind!(it, ExprVal, out);
            let rhs = self.parse_expr_val(rhs_cst);
            out.span = lhs.span.start..rhs.span.end;
            out.kind = infix(Box::new(lhs), Box::new(rhs));
            debug_assert!(it.peek().is_none());
            return out;
          }
          Some(CstElem::Tree(cst))
            if matches!(cst.kind, OperatorPostfix(_)) =>
          {
            match cst.kind {
              OperatorPostfix(op) => match op {
                PostfixOperator::ArrayIndex => {
                  let xpr = if let Some(CstElem::Tree(cst)) = it.next() {
                    self.parse_expr_val(cst)
                  } else {
                    dbg!("aaaa");
                    return out;
                  };
                  out.kind =
                    AstExprValKind::ArrayIndex(Box::new(lhs), Box::new(xpr));
                }
                PostfixOperator::FnCall => todo!(),
                PostfixOperator::Try => todo!(),
                PostfixOperator::As => todo!(),
                PostfixOperator::PostfixRangeExclusive => {
                  let end_expr = if let Some(CstElem::Tree(cst)) = it.next() {
                    self.parse_expr_val(cst)
                  } else {
                    dbg!("aaaa");
                    return out;
                  };
                  out.kind = AstExprValKind::RangeExclusive(
                    Box::new(lhs),
                    Box::new(end_expr),
                  );
                }
                PostfixOperator::PostfixRangeInclusive => {
                  let end_expr = if let Some(CstElem::Tree(cst)) = it.next() {
                    self.parse_expr_val(cst)
                  } else {
                    dbg!("aaaa");
                    return out;
                  };
                  out.kind = AstExprValKind::RangeInclusive(
                    Box::new(lhs),
                    Box::new(end_expr),
                  );
                }
              },
              _ => unimplemented!(),
            }
            expect_tk_kind!(it, ClBracket, out);
            debug_assert!(it.peek().is_none());
            return out;
          }
          _other => {
            return lhs;
          }
        }
      }
      Some(CstElem::Tree(cst))
        if matches!(cst.kind, CstKind::OperatorPrefix(_)) =>
      {
        match cst.kind {
          OperatorPrefix(op) => match op {
            PrefixOperator::Negative => todo!(),
            PrefixOperator::BitNot => todo!(),
            PrefixOperator::Dereference => {
              if let Some(CstElem::Tree(cst)) = it.next() {
                out.span = cst.span_within(&self.src);
                let i = self.parse_expr_val(cst);
                out.kind = AstExprValKind::Dereference(Box::new(i));
                return out;
              }
            }
            PrefixOperator::Reference => {
              if let Some(CstElem::Tree(cst)) = it.next() {
                out.span = cst.span_within(&self.src);
                let i = self.parse_expr_val(cst);
                out.kind = AstExprValKind::Reference(Box::new(i));
                return out;
              }
            }
            PrefixOperator::Return => todo!(),
            PrefixOperator::Break => {
              out.span = cst.span_within(&self.src);
              out.kind = AstExprValKind::Break;
              assert!(it.next().is_none());
            }
            PrefixOperator::PrefixRangeExclusive => todo!(),
            PrefixOperator::PrefixRangeInclusive => todo!(),
          },
          _ => unimplemented!(),
        }
        debug_assert!(it.peek().is_none());
        return out;
      }
      _other => {
        dbg!(&_other);
        out.span = cst.span_within(&self.src);
        debug_assert!(it.peek().is_none());
        return out;
      }
    }
  }
  pub fn parse_infix_operator(&self, cst: &Cst) -> Option<InfixMaker> {
    match cst.kind {
      CstKind::OperatorInfix(x) => Some(match x {
        InfixOperator::Path => AstExprValKind::Path,
        InfixOperator::Access => AstExprValKind::Access,
        InfixOperator::Mul => AstExprValKind::Mul,
        InfixOperator::Div => AstExprValKind::Div,
        InfixOperator::Rem => AstExprValKind::Rem,
        InfixOperator::Add => AstExprValKind::Add,
        InfixOperator::Sub => AstExprValKind::Sub,
        InfixOperator::ShiftLeft => AstExprValKind::ShiftLeft,
        InfixOperator::ShiftRight => AstExprValKind::ShiftRight,
        InfixOperator::BitAnd => AstExprValKind::BitAnd,
        InfixOperator::BitXor => AstExprValKind::BitXor,
        InfixOperator::BitOr => AstExprValKind::BitOr,
        InfixOperator::CmpEq => AstExprValKind::CmpEq,
        InfixOperator::CmpNe => AstExprValKind::CmpNe,
        InfixOperator::CmpLt => AstExprValKind::CmpLt,
        InfixOperator::CmpGt => AstExprValKind::CmpGt,
        InfixOperator::CmpLe => AstExprValKind::CmpLe,
        InfixOperator::CmpGe => AstExprValKind::CmpGe,
        InfixOperator::ConditionalAnd => AstExprValKind::ConditionalAnd,
        InfixOperator::ConditionalOr => AstExprValKind::ConditionalOr,
        InfixOperator::RangeExclusive => AstExprValKind::RangeExclusive,
        InfixOperator::RangeInclusive => AstExprValKind::RangeInclusive,
        InfixOperator::Assign => AstExprValKind::Assign,
        InfixOperator::AddAssign => AstExprValKind::AddAssign,
        InfixOperator::SubAssign => AstExprValKind::SubAssign,
        InfixOperator::MulAssign => AstExprValKind::MulAssign,
        InfixOperator::DivAssign => AstExprValKind::DivAssign,
        InfixOperator::RemAssign => AstExprValKind::RemAssign,
        InfixOperator::BitAndAssign => AstExprValKind::BitAndAssign,
        InfixOperator::BitOrAssign => AstExprValKind::BitOrAssign,
        InfixOperator::BitXorAssign => AstExprValKind::BitXorAssign,
        InfixOperator::ShiftLeftAssign => AstExprValKind::ShiftLeftAssign,
        InfixOperator::ShiftRightAssign => AstExprValKind::ShiftRightAssign,
      }),
      _ => None,
    }
  }
  pub fn parse_type_expr(&self, cst: &Cst) -> AstExprType {
    let mut out = AstExprType::default();
    out.span = cst.span_within(&self.src);
    if cst.kind != CstKind::ExprType {
      return out;
    }
    let mut it = cst.iter_important().peekable();
    match it.next() {
      Some(CstElem::Token(t)) if t.kind == Ident => {
        let (id, _) = self.token_id_span(*t);
        out.kind = AstExprTypeKind::Plain(id);
        if it.peek().is_none() {
          return out;
        } else {
          todo!()
        }
      }
      Some(CstElem::Token(t)) if t.kind == OpBracket => {
        let elem_cst = expect_cst_kind!(it, ExprType, out);
        expect_tk_kind!(it, Semicolon, out);
        let elem_ty = self.parse_type_expr(elem_cst);
        let len_cst = expect_cst_kind!(it, ExprVal, out);
        let length = self.parse_expr_val(len_cst);
        out.kind = AstExprTypeKind::Array {
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
    let mut out = AstModule { items: Vec::new() };

    for element in cst.iter_important() {
      let mut item = AstItem::default();
      match element {
        CstElem::Tree(tree) => {
          item.span = tree.span_within(&self.src);
          match tree.kind {
            CstKind::ItemStaticMmio => {
              item.kind = self.parse_static_mmio(tree);
            }
            CstKind::ItemConst => {
              item.kind = self.parse_constant(tree);
            }
            CstKind::ItemFunction => {
              item.kind = self.parse_function(tree);
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
    debug_assert_eq!(cst.kind, CstKind::ItemStaticMmio);
    let mut out = AstStaticMmio::default();
    let mut it = cst.iter_important();
    expect_tk_kind!(it, KwStatic, ErrAstItemKind);
    expect_tk_kind!(it, KwMmio, ErrAstItemKind);
    expect_tk_kind!(it, OpParen, ErrAstItemKind);
    out.address =
      self.parse_expr_val(expect_cst_kind!(it, ExprVal, ErrAstItemKind));
    expect_tk_kind!(it, ClParen, ErrAstItemKind);
    let (id, span) =
      self.token_id_span(expect_tk_kind!(it, Ident, ErrAstItemKind));
    out.name = id;
    out.name_span = span;
    expect_tk_kind!(it, Colon, ErrAstItemKind);
    out.ty =
      self.parse_type_expr(expect_cst_kind!(it, ExprType, ErrAstItemKind));
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
      self.parse_type_expr(expect_cst_kind!(it, ExprType, ErrAstItemKind));
    expect_tk_kind!(it, Equal, ErrAstItemKind);
    out.xpr =
      self.parse_expr_val(expect_cst_kind!(it, ExprVal, ErrAstItemKind));
    expect_tk_kind!(it, Semicolon, ErrAstItemKind);
    AstItemKind::Constant(out)
  }
  fn parse_function(&self, cst: &Cst) -> AstItemKind {
    debug_assert_eq!(cst.kind, CstKind::ItemFunction);
    let mut out = AstFunction::default();
    let mut it = cst.iter_important();
    expect_tk_kind!(it, KwFn, ErrAstItemKind);
    let (id, span) =
      self.token_id_span(expect_tk_kind!(it, Ident, ErrAstItemKind));
    out.name = id;
    out.name_span = span;
    out.arguments = self.parse_argument_list(expect_cst_kind!(
      it,
      ArgumentList,
      ErrAstItemKind
    ));
    out.return_ty =
      self.parse_return_ty(expect_cst_kind!(it, ReturnType, ErrAstItemKind));
    out.body = self.parse_body(expect_cst_kind!(it, Body, ErrAstItemKind));
    AstItemKind::Function(out)
  }

  fn parse_argument_list(&self, cst: &Cst) -> Vec<AstFunctionArgument> {
    debug_assert_eq!(cst.kind, CstKind::ArgumentList);
    // TODO: parse args
    Vec::new()
  }

  fn parse_return_ty(&self, cst: &Cst) -> AstExprType {
    debug_assert_eq!(cst.kind, CstKind::ReturnType);
    let mut out = AstExprType::default();
    out.span = cst.span_within(&self.src);
    let mut it = cst.iter_important();
    match it.next() {
      None => {
        out.kind = AstExprTypeKind::Plain(StrId::from("()"));
      }
      _ => todo!(),
    }
    out
  }

  fn parse_body(&self, cst: &Cst) -> AstBody {
    debug_assert_eq!(cst.kind, CstKind::Body);
    let mut out = AstBody::default();
    let mut it = cst.iter_important();
    expect_tk_kind!(it, OpBrace, out);
    for elem in it {
      match elem {
        CstElem::Token(token) if token.kind == ClBrace => break,
        CstElem::Tree(cst) => {
          let mut stmt = AstStatement::default();
          stmt.span = cst.span_within(&self.src);
          stmt.kind = match cst.kind {
            CstKind::StmtEmpty => continue,
            CstKind::StmtExpression => self.parse_stmt_expression(cst),
            CstKind::StmtLet => self.parse_stmt_let(cst),
            _other => {
              dbg!(&_other);
              AstStatementKind::ErrAstStatementKind
            }
          };
          out.statements.push(stmt);
        }
        _other => {
          dbg!(&_other);
        }
      }
    }
    out
  }

  fn parse_stmt_expression(&self, cst: &Cst) -> AstStatementKind {
    debug_assert_eq!(cst.kind, CstKind::StmtExpression);
    //dbg!(cst);
    let mut it = cst.iter_important();
    let val_expr_cst = expect_cst_kind!(it, ExprVal, ErrAstStatementKind);
    let val_expr = self.parse_expr_val(val_expr_cst);
    //expect_tk_kind!(it, Semicolon, ErrAstStatementKind);
    AstStatementKind::Expression(val_expr)
  }

  fn parse_stmt_let(&self, cst: &Cst) -> AstStatementKind {
    debug_assert_eq!(cst.kind, CstKind::StmtLet);
    let mut it = cst.iter_important();
    expect_tk_kind!(it, KwLet, ErrAstStatementKind);
    let (id, span) =
      self.token_id_span(expect_tk_kind!(it, Ident, ErrAstStatementKind));
    let pattern = AstExprVal { span, kind: AstExprValKind::Identifier(id) };
    expect_tk_kind!(it, Equal, ErrAstStatementKind);
    let val_cst = expect_cst_kind!(it, ExprVal, ErrAstStatementKind);
    let xpr = self.parse_expr_val(val_cst);
    expect_tk_kind!(it, Semicolon, ErrAstStatementKind);
    AstStatementKind::Let(AstLet { pattern, xpr })
  }
}
