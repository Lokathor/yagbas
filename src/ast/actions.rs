#![allow(clippy::ptr_arg)]
#![allow(unused_variables)]

use std::path::PathBuf;

use crate::{
  Span,
  ast::{
    FunctionArg, Item, ItemKind, Module, Pattern, PatternKind, Statement,
    StatementKind, StaticKind, TypeExpr, TypeExprKind, ValueExpr,
    ValueExprKind, parser::AstParser,
  },
  cst::{
    Cst, CstElem,
    CstKind::{self},
  },
  operators::{BinOpKind, PostOp, PrefOp, UnOpKind},
  tokenizer::TokenKind::{
    self, ClBrace, ClBracket, ClParen, Colon, Equal, KwConst, KwElse, KwFn,
    KwFor, KwIf, KwIn, KwLet, KwLoop, KwMmio, KwRam, KwRom, KwStatic,
    MinusGreater, OpBrace, OpBracket, OpParen, Semicolon,
  },
};

/// * `($p:expr, $it:expr, $eoi_span:expr, $x:expr)`
macro_rules! basic_fixed_token {
  ($p:expr, $it:expr, $eoi_span:expr, $x:expr) => {{
    match $it.next() {
      Some(CstElem::FixedToken(x, opt_span)) if *x == $x => *opt_span,
      Some(other) => {
        let x_str = $x.fixed_str().unwrap();
        $p.error_at(
          other.try_span().unwrap_or_default(),
          format!("Expected `{x_str}`, got: {other:?}"),
        );
        None
      }
      None => {
        let x_str = $x.fixed_str().unwrap();
        $p.error_at(
          $eoi_span,
          format!("Expected `{x_str}`, got EndOfGrouping"),
        );
        None
      }
    }
  }};
}

pub fn parse_ast_module(
  p: &mut AstParser, file_origin: PathBuf, cst: &Cst,
) -> Module {
  debug_assert_eq!(cst.kind, CstKind::Module);
  //
  let mut out = Module::default();
  out.file_origin = file_origin.clone();
  for elem in &cst.elements {
    match elem {
      CstElem::SubTree(cst) if cst.kind == CstKind::Item => {
        let item = parse_ast_item(p, file_origin.clone(), cst);
        out.items.push(item);
      }
      other => {
        p.error_at(
          cst.try_span().unwrap_or_default(),
          format!("InternalError: Bad Data From Cst Parser: {other:?}"),
        );
        continue;
      }
    }
  }

  out
}

fn parse_ast_item(p: &mut AstParser, file_origin: PathBuf, cst: &Cst) -> Item {
  debug_assert_eq!(cst.kind, CstKind::Item);
  //
  let mut out = Item::default();
  out.file_origin = file_origin.clone();
  out.span = cst.try_span().unwrap_or_default();
  match cst.elements.first() {
    Some(CstElem::FixedToken(KwFn, span)) => {
      parse_ast_function(p, cst, &mut out);
    }
    Some(CstElem::FixedToken(KwConst, span)) => {
      parse_ast_constant(p, cst, &mut out);
    }
    Some(CstElem::FixedToken(KwStatic, span)) => {
      parse_ast_static(p, cst, &mut out);
    }
    other => {
      p.error_at(
        out.span,
        format!("ParserIncomplete: Unknown Item: {other:?}"),
      );
      return out;
    }
  }
  out
}

fn parse_ast_function(p: &mut AstParser, cst: &Cst, out: &mut Item) {
  debug_assert_eq!(
    cst.elements.first().unwrap().fixed_token().unwrap().0,
    KwFn
  );
  //
  let mut it = cst.elements.iter().peekable();
  let mut args = Default::default();
  let mut ret_ty = Default::default();
  let mut statements = Default::default();

  basic_fixed_token!(p, it, out.span, KwFn);

  match it.next() {
    Some(CstElem::Identifier(name, opt_span)) => {
      out.name = name.clone();
      out.name_span = opt_span.unwrap_or_default();
    }
    other => {
      p.error_at(
        cst.try_span().unwrap_or_default(),
        format!("Expected Identfier: {other:?}"),
      );
    }
  };

  match it.next() {
    Some(CstElem::SubTree(cst)) if cst.kind == CstKind::Parens => {
      args = parse_ast_function_args(p, cst);
    }
    other => {
      p.error_at(
        cst.try_span().unwrap_or_default(),
        format!("Expected Parens Group: {other:?}"),
      );
    }
  };

  if matches!(it.peek(), Some(&CstElem::FixedToken(MinusGreater, _))) {
    let _ = it.next();
    match it.next() {
      Some(CstElem::SubTree(ty_expr)) if ty_expr.kind == CstKind::ExprType => {
        ret_ty = parse_type_expr(p, ty_expr);
      }
      other => {
        p.error_at(
          cst.try_span().unwrap_or_default(),
          format!("Expected Type Expr: {other:?}"),
        );
      }
    }
  } else {
    ret_ty = TypeExpr {
      span: Span::default(),
      kind: Box::new(TypeExprKind::Simple(String::from("()"))),
    }
  }

  match it.next() {
    Some(CstElem::SubTree(cst)) if cst.kind == CstKind::ExprValue => {
      statements = parse_value_expr_body(p, cst);
    }
    other => {
      p.error_at(
        cst.try_span().unwrap_or_default(),
        format!("Expected function body: {other:?}"),
      );
    }
  };

  out.kind = ItemKind::Function { args, ret_ty, statements };

  for i in it {
    dbg!(&i);
  }
}

fn parse_ast_constant(p: &mut AstParser, cst: &Cst, out: &mut Item) {
  debug_assert_eq!(
    cst.elements.first().unwrap().fixed_token().unwrap().0,
    KwConst
  );
  //
  let mut it = cst.elements.iter().peekable();
  let mut type_decl = TypeExpr::default();
  let mut value_decl = ValueExpr::default();

  basic_fixed_token!(p, it, out.span, KwConst);

  match it.next() {
    Some(CstElem::Identifier(name, opt_span)) => {
      out.name = name.clone();
      out.name_span = opt_span.unwrap_or_default();
    }
    other => {
      p.error_at(
        cst.try_span().unwrap_or_default(),
        format!("Expected Identfier: {other:?}"),
      );
    }
  };

  basic_fixed_token!(p, it, out.span, Colon);

  match it.next() {
    Some(CstElem::SubTree(cst)) if cst.kind == CstKind::ExprType => {
      type_decl = parse_type_expr(p, cst);
    }
    other => {
      p.error_at(
        cst.try_span().unwrap_or_default(),
        format!("Expected Type Expr: {other:?}"),
      );
    }
  }

  basic_fixed_token!(p, it, out.span, Equal);

  match it.next() {
    Some(CstElem::SubTree(cst)) if cst.kind == CstKind::ExprValue => {
      value_decl = parse_value_expr(p, cst);
    }
    other => {
      p.error_at(
        cst.try_span().unwrap_or_default(),
        format!("Expected Value Expr: {other:?}"),
      );
    }
  }
  match it.next() {
    Some(CstElem::FixedToken(Semicolon, _)) => {}
    other => {
      p.error_at(out.span, format!("Expected `;`: {other:?}"));
    }
  }

  out.kind = ItemKind::Constant { type_decl, value_decl };

  for elem in it {
    println!("== {elem:?}");
  }
}

fn parse_ast_static(p: &mut AstParser, cst: &Cst, out: &mut Item) {
  debug_assert_eq!(
    cst.elements.first().unwrap().fixed_token().unwrap().0,
    KwStatic
  );
  //
  let mut it = cst.elements.iter().peekable();
  let mut kind = StaticKind::default();

  basic_fixed_token!(p, it, out.span, KwStatic);

  match it.next() {
    Some(CstElem::FixedToken(KwMmio, _)) => {
      let mut location = ValueExpr::default();
      let mut type_decl = TypeExpr::default();

      basic_fixed_token!(p, it, out.span, OpParen);

      match it.next() {
        Some(CstElem::SubTree(cst)) if cst.kind == CstKind::ExprValue => {
          location = parse_value_expr(p, cst);
        }
        other => {
          p.error_at(
            cst.try_span().unwrap_or_default(),
            format!("Expected Value Expr: {other:?}"),
          );
        }
      }

      basic_fixed_token!(p, it, out.span, ClParen);

      match it.next() {
        Some(CstElem::Identifier(name, opt_span)) => {
          out.name = name.clone();
          out.name_span = opt_span.unwrap_or_default();
        }
        other => {
          p.error_at(
            cst.try_span().unwrap_or_default(),
            format!("Expected Identfier: {other:?}"),
          );
        }
      };

      basic_fixed_token!(p, it, out.span, Colon);

      match it.next() {
        Some(CstElem::SubTree(cst)) if cst.kind == CstKind::ExprType => {
          type_decl = parse_type_expr(p, cst);
        }
        other => {
          p.error_at(
            cst.try_span().unwrap_or_default(),
            format!("Expected Type Expr: {other:?}"),
          );
        }
      }
      out.kind =
        ItemKind::Static { kind: StaticKind::Mmio { location, type_decl } };
    }
    Some(CstElem::FixedToken(KwRam, _)) => {
      let mut type_decl = TypeExpr::default();
      let mut init = ValueExpr::default();
      match it.next() {
        Some(CstElem::Identifier(name, opt_span)) => {
          out.name = name.clone();
          out.name_span = opt_span.unwrap_or_default();
        }
        other => {
          p.error_at(
            cst.try_span().unwrap_or_default(),
            format!("Expected Identfier: {other:?}"),
          );
        }
      };

      basic_fixed_token!(p, it, out.span, Colon);

      match it.next() {
        Some(CstElem::SubTree(cst)) if cst.kind == CstKind::ExprType => {
          type_decl = parse_type_expr(p, cst);
        }
        other => {
          p.error_at(
            cst.try_span().unwrap_or_default(),
            format!("Expected Type Expr: {other:?}"),
          );
        }
      }

      basic_fixed_token!(p, it, out.span, Equal);

      match it.next() {
        Some(CstElem::SubTree(cst)) if cst.kind == CstKind::ExprValue => {
          init = parse_value_expr(p, cst);
        }
        other => {
          p.error_at(
            cst.try_span().unwrap_or_default(),
            format!("Expected Value Expr: {other:?}"),
          );
        }
      }
      out.kind = ItemKind::Static { kind: StaticKind::Ram { type_decl, init } };
    }
    Some(CstElem::FixedToken(KwRom, _)) => {
      let mut type_decl = TypeExpr::default();
      let mut data = ValueExpr::default();
      match it.next() {
        Some(CstElem::Identifier(name, opt_span)) => {
          out.name = name.clone();
          out.name_span = opt_span.unwrap_or_default();
        }
        other => {
          p.error_at(
            cst.try_span().unwrap_or_default(),
            format!("Expected Identfier: {other:?}"),
          );
        }
      };

      basic_fixed_token!(p, it, out.span, Colon);

      match it.next() {
        Some(CstElem::SubTree(cst)) if cst.kind == CstKind::ExprType => {
          type_decl = parse_type_expr(p, cst);
        }
        other => {
          p.error_at(
            cst.try_span().unwrap_or_default(),
            format!("Expected Type Expr: {other:?}"),
          );
        }
      }

      basic_fixed_token!(p, it, out.span, Equal);

      match it.next() {
        Some(CstElem::SubTree(cst)) if cst.kind == CstKind::ExprValue => {
          data = parse_value_expr(p, cst);
        }
        other => {
          p.error_at(
            cst.try_span().unwrap_or_default(),
            format!("Expected Value Expr: {other:?}"),
          );
        }
      }
      out.kind = ItemKind::Static { kind: StaticKind::Rom { type_decl, data } };
    }
    other => {
      todo!("{other:?}");
    }
  }

  basic_fixed_token!(p, it, out.span, Semicolon);

  for i in it {
    dbg!(&i);
  }
}

fn parse_type_expr(p: &mut AstParser, cst: &Cst) -> TypeExpr {
  let mut it = cst.elements.iter();
  let mut out = TypeExpr::default();
  out.span = cst.try_span().unwrap_or_default();

  match it.next() {
    Some(CstElem::Identifier(name, opt_span)) => {
      out.span = opt_span.unwrap_or_default();
      out.kind = Box::new(TypeExprKind::Simple(name.clone()));
    }
    Some(CstElem::FixedToken(OpBracket, _)) => {
      let mut elem_ty = TypeExpr::default();
      let mut elem_count = ValueExpr::default();
      match it.next() {
        Some(CstElem::SubTree(cst)) if cst.kind == CstKind::ExprType => {
          elem_ty = parse_type_expr(p, cst);
        }
        other => {
          p.error_at(
            cst.try_span().unwrap_or_default(),
            format!("Expected Type Expr: {other:?}"),
          );
        }
      }

      basic_fixed_token!(p, it, out.span, Semicolon);

      match it.next() {
        Some(CstElem::SubTree(cst)) if cst.kind == CstKind::ExprValue => {
          elem_count = parse_value_expr(p, cst);
        }
        other => {
          p.error_at(
            cst.try_span().unwrap_or_default(),
            format!("Expected Value Expr: {other:?}"),
          );
        }
      }

      basic_fixed_token!(p, it, out.span, ClBracket);

      out.kind = Box::new(TypeExprKind::Array { elem_ty, elem_count });
    }
    other => {
      p.error_at(
        cst.try_span().unwrap_or_default(),
        format!("Expected Identifier or `[`: {other:?}"),
      );
    }
  }

  for i in it {
    dbg!(&i);
  }

  out
}

fn parse_value_expr(p: &mut AstParser, cst: &Cst) -> ValueExpr {
  debug_assert_eq!(cst.kind, CstKind::ExprValue);
  //
  let span = cst.try_span().unwrap_or_default();
  let mut it = cst.elements.iter();
  match it.next() {
    Some(CstElem::FixedToken(OpBrace, _)) => ValueExpr {
      span,
      kind: Box::new(ValueExprKind::Block {
        statements: parse_value_expr_body(p, cst),
      }),
    },
    Some(CstElem::FixedToken(KwFor, _)) => parse_value_expr_for(p, cst),
    Some(CstElem::FixedToken(KwLoop, _)) => parse_value_expr_loop(p, cst),
    Some(CstElem::FixedToken(KwIf, _)) => parse_value_expr_if(p, cst),
    Some(CstElem::Identifier(string, opt_span)) => ValueExpr {
      span: opt_span.unwrap_or_default(),
      kind: Box::new(ValueExprKind::Identifier(string.clone())),
    },
    Some(CstElem::LitNumber(string, opt_span)) => ValueExpr {
      span: opt_span.unwrap_or_default(),
      kind: Box::new(ValueExprKind::LiteralNumber(string.clone())),
    },
    Some(CstElem::LitString(string, opt_span)) => ValueExpr {
      span: opt_span.unwrap_or_default(),
      kind: Box::new(ValueExprKind::LiteralString(string.clone())),
    },
    Some(CstElem::SubTree(cst)) => match cst.kind {
      CstKind::ExprValue => {
        let left = parse_value_expr(p, cst);
        match it.next() {
          Some(CstElem::SubTree(cst))
            if matches!(cst.kind, CstKind::InfiOp(op)) =>
          {
            let op = match cst.kind {
              CstKind::InfiOp(op) => BinOpKind::from(op),
              _ => unimplemented!(),
            };
            match it.next() {
              Some(CstElem::SubTree(cst)) if cst.kind == CstKind::ExprValue => {
                let right = parse_value_expr(p, cst);
                ValueExpr {
                  span,
                  kind: Box::new(ValueExprKind::BinOp { left, op, right }),
                }
              }
              other => {
                p.error_at(
                  cst.try_span().unwrap_or_default(),
                  format!("Expected Right Hand Side Expression: {other:?}"),
                );
                let mut out = ValueExpr::default();
                out.span = cst.try_span().unwrap_or_default();
                out
              }
            }
          }
          Some(CstElem::SubTree(cst))
            if matches!(cst.kind, CstKind::PostOp(op)) =>
          {
            match cst.kind {
              CstKind::PostOp(PostOp::PostfixRangeExclusive) => {
                match it.next() {
                  Some(CstElem::SubTree(cst))
                    if cst.kind == CstKind::ExprValue =>
                  {
                    let op = BinOpKind::RangeExclusive;
                    let right = parse_value_expr(p, cst);
                    ValueExpr {
                      span,
                      kind: Box::new(ValueExprKind::BinOp { left, op, right }),
                    }
                  }
                  None => {
                    let op = UnOpKind::PostfixRangeExclusive;
                    ValueExpr {
                      span,
                      kind: Box::new(ValueExprKind::UnOp { op, operand: left }),
                    }
                  }
                  other => {
                    p.error_at(
                      cst.try_span().unwrap_or_default(),
                      format!("Expected Right Hand Side Expression: {other:?}"),
                    );
                    let mut out = ValueExpr::default();
                    out.span = cst.try_span().unwrap_or_default();
                    out
                  }
                }
              }
              CstKind::PostOp(PostOp::ArrayIndex) => match it.next() {
                Some(CstElem::SubTree(cst))
                  if cst.kind == CstKind::ExprValue =>
                {
                  let op = BinOpKind::ArrayIndex;
                  let right = parse_value_expr(p, cst);
                  match it.next() {
                    Some(CstElem::FixedToken(ClBracket, _)) => {}
                    other => {
                      p.error_at(
                        right.span,
                        format!("Expected Closing Bracket: {other:?}"),
                      );
                    }
                  }
                  ValueExpr {
                    span,
                    kind: Box::new(ValueExprKind::BinOp { left, op, right }),
                  }
                }
                other => {
                  todo!("{other:?}")
                }
              },
              other => todo!("{other:?}"),
            }
          }
          other => {
            todo!("Found LHS {left:?} then unknown {other:?}");
          }
        }
      }
      CstKind::PrefOp(PrefOp::Dereference) => match it.next() {
        Some(CstElem::SubTree(cst))
          if matches!(cst.kind, CstKind::ExprValue) =>
        {
          ValueExpr {
            span,
            kind: Box::new(ValueExprKind::UnOp {
              op: UnOpKind::Dereference,
              operand: parse_value_expr(p, cst),
            }),
          }
        }
        other => {
          p.error_at(
            cst.try_span().unwrap_or_default(),
            format!("Unknown after LHS Expression: {other:?}"),
          );
          let mut out = ValueExpr::default();
          out.span = cst.try_span().unwrap_or_default();
          out
        }
      },
      CstKind::PrefOp(PrefOp::Reference) => match it.next() {
        Some(CstElem::SubTree(cst))
          if matches!(cst.kind, CstKind::ExprValue) =>
        {
          ValueExpr {
            span,
            kind: Box::new(ValueExprKind::UnOp {
              op: UnOpKind::Reference,
              operand: parse_value_expr(p, cst),
            }),
          }
        }
        other => {
          p.error_at(
            cst.try_span().unwrap_or_default(),
            format!("Unknown after LHS Expression: {other:?}"),
          );
          let mut out = ValueExpr::default();
          out.span = cst.try_span().unwrap_or_default();
          out
        }
      },
      CstKind::PrefOp(PrefOp::Break) => match it.next() {
        None => ValueExpr {
          span,
          kind: Box::new(ValueExprKind::Break { label: None, value: None }),
        },
        Some(elem) => todo!("{elem:?}"),
      },
      CstKind::PrefOp(op) => {
        todo!("Unhandled PrefixOp {op:?}")
      }
      other => {
        p.error_at(
          span,
          format!("Unknown SubTree Expression Start Kind: {other:?}"),
        );
        let mut out = ValueExpr::default();
        out.span = cst.try_span().unwrap_or_default();
        out
      }
    },
    other => {
      p.error_at(span, format!("Unknown Expression Start: {other:?}"));
      let mut out = ValueExpr::default();
      out.span = cst.try_span().unwrap_or_default();
      out
    }
  }
}

fn parse_value_expr_for(p: &mut AstParser, cst: &Cst) -> ValueExpr {
  debug_assert_eq!(cst.kind, CstKind::ExprValue);
  debug_assert_eq!(
    cst.elements.first().unwrap().fixed_token().unwrap().0,
    KwFor
  );
  //
  let mut it = cst.elements.iter();
  let mut label = None;
  let mut step_var = Pattern::default();
  let mut range = ValueExpr::default();
  let mut statements = Vec::new();
  let mut out = ValueExpr::default();
  out.span = cst.try_span().unwrap_or_default();

  basic_fixed_token!(p, it, out.span, KwFor);

  match it.next() {
    Some(CstElem::SubTree(cst)) if cst.kind == CstKind::Pattern => {
      step_var = parse_pattern(p, cst);
    }
    other => {
      p.error_at(out.span, format!("Expected Pattern: {other:?}"));
    }
  }

  basic_fixed_token!(p, it, out.span, KwIn);

  match it.next() {
    Some(CstElem::SubTree(cst)) if cst.kind == CstKind::ExprValue => {
      range = parse_value_expr(p, cst);
    }
    other => {
      p.error_at(out.span, format!("Expected range expression: {other:?}"));
    }
  }
  match it.next() {
    Some(CstElem::SubTree(cst)) if cst.kind == CstKind::ExprValue => {
      statements = parse_value_expr_body(p, cst);
    }
    other => {
      p.error_at(out.span, format!("Expected body: {other:?}"));
    }
  }

  for i in it {
    dbg!(&i);
  }

  out.kind =
    Box::new(ValueExprKind::For { label, step_var, range, statements });
  out
}

fn parse_value_expr_loop(p: &mut AstParser, cst: &Cst) -> ValueExpr {
  debug_assert_eq!(cst.kind, CstKind::ExprValue);
  debug_assert_eq!(
    cst.elements.first().unwrap().fixed_token().unwrap().0,
    KwLoop
  );
  //
  let mut it = cst.elements.iter();
  let mut label = None;
  let mut step_var = Pattern::default();
  let mut range = ValueExpr::default();
  let mut statements = Vec::new();
  let mut out = ValueExpr::default();
  out.span = cst.try_span().unwrap_or_default();

  basic_fixed_token!(p, it, out.span, KwLoop);

  match it.next() {
    Some(CstElem::SubTree(cst)) if cst.kind == CstKind::ExprValue => {
      statements = parse_value_expr_body(p, cst);
    }
    other => {
      p.error_at(out.span, format!("Expected body: {other:?}"));
    }
  }

  for i in it {
    dbg!(&i);
  }

  out.kind = Box::new(ValueExprKind::Loop { label, statements });
  out
}

fn parse_value_expr_if(p: &mut AstParser, cst: &Cst) -> ValueExpr {
  debug_assert_eq!(cst.kind, CstKind::ExprValue);
  debug_assert_eq!(
    cst.elements.first().unwrap().fixed_token().unwrap().0,
    KwIf
  );
  //
  let mut it = cst.elements.iter().peekable();
  let mut condition = ValueExpr::default();
  let mut when_true = Vec::new();
  let mut when_false = Vec::new();
  let mut out = ValueExpr::default();
  out.span = cst.try_span().unwrap_or_default();

  basic_fixed_token!(p, it, out.span, KwIf);

  match it.next() {
    Some(CstElem::SubTree(cst)) if cst.kind == CstKind::ExprValue => {
      condition = parse_value_expr(p, cst);
    }
    other => {
      p.error_at(out.span, format!("Expected condition : {other:?}"));
    }
  }
  match it.next() {
    Some(CstElem::SubTree(cst)) if cst.kind == CstKind::ExprValue => {
      when_true = parse_value_expr_body(p, cst);
    }
    other => {
      p.error_at(out.span, format!("Expected body: {other:?}"));
    }
  }
  if matches!(it.peek(), Some(CstElem::FixedToken(KwElse, _))) {
    basic_fixed_token!(p, it, out.span, KwElse);

    match it.next() {
      Some(CstElem::SubTree(cst)) if cst.kind == CstKind::ExprValue => {
        when_false = parse_value_expr_body(p, cst);
      }
      other => {
        p.error_at(out.span, format!("Expected body: {other:?}"));
      }
    }
  }

  for i in it {
    dbg!(&i);
  }

  out.kind = Box::new(ValueExprKind::If { condition, when_true, when_false });
  out
}

/// Returns the statements of a cst holding a ValExpr body (`{ }`).
///
/// The return value is a vec so that this can be shared between an "actual"
/// body as well as with the other expression forms that have expression blocks.
fn parse_value_expr_body(p: &mut AstParser, cst: &Cst) -> Vec<Statement> {
  debug_assert_eq!(cst.kind, CstKind::ExprValue);
  debug_assert_eq!(
    cst.elements.first().unwrap().fixed_token().unwrap().0,
    OpBrace
  );
  //
  let mut it = cst.elements.iter();
  let mut statements = Vec::new();

  basic_fixed_token!(p, it, cst.try_span().unwrap_or_default(), OpBrace);

  loop {
    match it.next() {
      Some(CstElem::FixedToken(ClBrace, _span)) => {
        break;
      }
      Some(CstElem::SubTree(cst)) if cst.kind == CstKind::Statement => {
        statements.push(parse_statement(p, cst));
      }
      other => {
        p.error_at(
          cst.try_span().unwrap_or_default(),
          format!("Expected Statement: {other:?}"),
        );
      }
    }
  }

  statements
}

fn parse_statement(p: &mut AstParser, cst: &Cst) -> Statement {
  debug_assert_eq!(cst.kind, CstKind::Statement);
  //
  match cst.elements.first() {
    Some(CstElem::FixedToken(KwLet, _)) => parse_statement_let(p, cst),
    Some(CstElem::SubTree(cst)) if cst.kind == CstKind::ExprValue => {
      let x = parse_value_expr(p, cst);
      Statement {
        span: cst.try_span().unwrap_or_default(),
        kind: Box::new(StatementKind::Expression(x)),
      }
    }
    _ => {
      let mut out = Statement::default();
      out.span = cst.try_span().unwrap_or_default();
      let mut it = cst.elements.iter();

      for i in it {
        dbg!(&i);
      }

      out
    }
  }
}

fn parse_statement_let(p: &mut AstParser, cst: &Cst) -> Statement {
  debug_assert_eq!(cst.kind, CstKind::Statement);
  debug_assert_eq!(
    cst.elements.first().unwrap().fixed_token().unwrap().0,
    KwLet
  );
  //
  let mut out = Statement::default();
  out.span = cst.try_span().unwrap_or_default();
  let mut it = cst.elements.iter().peekable();

  let mut pattern = Pattern::default();
  let mut type_decl = None;
  let mut initializer = None;

  basic_fixed_token!(p, it, out.span, KwLet);

  match it.next() {
    Some(CstElem::SubTree(cst)) if cst.kind == CstKind::Pattern => {
      pattern = parse_pattern(p, cst);
    }
    other => {
      p.error_at(out.span, format!("Expected Pattern: {other:?}"));
    }
  }

  if matches!(it.peek(), Some(CstElem::FixedToken(Colon, _))) {
    basic_fixed_token!(p, it, out.span, Colon);

    match it.next() {
      Some(CstElem::SubTree(cst)) if cst.kind == CstKind::ExprType => {
        type_decl = Some(parse_type_expr(p, cst));
      }
      other => {
        p.error_at(out.span, format!("Expected Type Expr: {other:?}"));
      }
    }
  }

  if matches!(it.peek(), Some(CstElem::FixedToken(Equal, _))) {
    basic_fixed_token!(p, it, out.span, Equal);

    match it.next() {
      Some(CstElem::SubTree(cst)) if cst.kind == CstKind::ExprValue => {
        initializer = Some(parse_value_expr(p, cst));
      }
      other => {
        p.error_at(out.span, format!("Expected Value Expr: {other:?}"));
      }
    }
  }

  basic_fixed_token!(p, it, out.span, Semicolon);

  for i in it {
    println!("LET!! {i:?}");
  }

  out.kind = Box::new(StatementKind::Let { pattern, type_decl, initializer });
  out
}

fn parse_ast_function_args(p: &mut AstParser, cst: &Cst) -> Vec<FunctionArg> {
  debug_assert_eq!(cst.kind, CstKind::Parens);
  //
  let mut out = Vec::new();
  let mut it = cst.elements.iter();
  let op_paren = it.next();
  debug_assert_eq!(
    op_paren.unwrap().fixed_token().unwrap().0,
    TokenKind::OpParen
  );
  loop {
    match it.next() {
      Some(CstElem::FixedToken(ClParen, _span)) => {
        debug_assert!(it.next().is_none());
        return out;
      }
      Some(CstElem::SubTree(pat_tree)) if pat_tree.kind == CstKind::Pattern => {
        let mut pattern = parse_pattern(p, pat_tree);
        let mut type_decl = TypeExpr::default();
        match it.next() {
          Some(CstElem::FixedToken(Colon, _span)) => {}
          other => {
            p.error_at(
              cst.try_span().unwrap_or_default(),
              format!("Expected Colon: {other:?}"),
            );
          }
        }
        match it.next() {
          Some(CstElem::SubTree(ty_tree))
            if ty_tree.kind == CstKind::ExprType =>
          {
            type_decl = parse_type_expr(p, ty_tree);
          }
          other => {
            p.error_at(
              cst.try_span().unwrap_or_default(),
              format!("Expected Type Expr: {other:?}"),
            );
          }
        }
        out.push(FunctionArg { pattern, type_decl });
      }
      other => {
        p.error_at(
          cst.try_span().unwrap_or_default(),
          format!("Expected Pattern or Close Paren: {other:?}"),
        );
      }
    }
  }
}

fn parse_pattern(p: &mut AstParser, cst: &Cst) -> Pattern {
  debug_assert_eq!(cst.kind, CstKind::Pattern, "{cst}");
  //
  match cst.elements.as_slice() {
    [CstElem::Identifier(name, opt_span)] => Pattern {
      span: opt_span.unwrap_or_default(),
      kind: PatternKind::Simple(name.clone()),
    },
    other => {
      p.error_at(
        cst.try_span().unwrap_or_default(),
        format!("Expected Pattern: {other:?}"),
      );
      Pattern {
        span: cst.try_span().unwrap_or_default(),
        kind: PatternKind::ErrPatternKind,
      }
    }
  }
}
