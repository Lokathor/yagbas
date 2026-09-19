#![allow(clippy::ptr_arg)]
#![allow(unused_variables)]

use std::path::PathBuf;

use crate::{
  ast::{
    AstError, FunctionArg, Item, ItemKind, Module, Pattern, PatternKind,
    Statement, StatementKind, TypeExpr, TypeExprKind, ValueExpr, ValueExprKind,
  },
  cst::{
    Cst, CstElem,
    CstKind::{self},
  },
  operators::{BinOpKind, PostfixOperator, UnOpKind},
  tokenizer::TokenKind::{
    self, ClBrace, ClParen, Colon, Equal, KwFn, KwFor, KwIn, KwLet,
    MinusGreater, OpBrace, Semicolon,
  },
};

pub fn parse_ast_module(
  errors: &mut Vec<AstError>, file_origin: PathBuf, cst: &Cst,
) -> Module {
  debug_assert_eq!(cst.kind, CstKind::Module);
  //
  let mut out = Module::default();
  out.file_origin = file_origin.clone();
  for elem in &cst.elements {
    match elem {
      CstElem::SubTree(cst) if cst.kind == CstKind::Item => {
        let item = parse_ast_item(errors, file_origin.clone(), cst);
        out.items.push(item);
      }
      otherwise => {
        errors.push(AstError::CstParserMadeModuleWithBadData(format!(
          "{otherwise:?}"
        )));
        continue;
      }
    }
  }

  out
}

fn parse_ast_item(
  errors: &mut Vec<AstError>, file_origin: PathBuf, cst: &Cst,
) -> Item {
  debug_assert_eq!(cst.kind, CstKind::Item);
  //
  let mut out = Item::default();
  out.file_origin = file_origin.clone();
  out.span = cst.try_span().unwrap_or_default();
  match cst.elements.first() {
    Some(CstElem::FixedToken(KwFn, span)) => {
      parse_ast_function(errors, cst, &mut out);
    }
    _other => {
      dbg!(_other);
      return out;
    }
  }
  out
}

fn parse_ast_function(errors: &mut Vec<AstError>, cst: &Cst, out: &mut Item) {
  debug_assert_eq!(
    cst.elements.first().unwrap().fixed_token().unwrap().0,
    KwFn
  );
  //
  let mut it = cst.elements.iter();
  let mut args = Default::default();
  let mut ret_ty = Default::default();
  let mut statements = Default::default();

  // keyword, already checked by caller and also debug asserted for.
  let _ = it.next();

  match it.next() {
    Some(CstElem::Identifier(name, opt_span)) => {
      out.name = name.clone();
      out.name_span = opt_span.unwrap_or_default();
    }
    Some(other) => {
      errors.push(AstError::ErrGeneric(
        other.try_span().unwrap_or_default(),
        format!("Expected Identifier got {other:?}"),
      ));
    }
    None => {
      errors.push(AstError::ErrGeneric(
        cst.try_span().unwrap_or_default(),
        format!("Expected Identifier, but no input."),
      ));
      out.kind = ItemKind::Function { args, ret_ty, statements };
      return;
    }
  };

  match it.next() {
    Some(CstElem::SubTree(cst)) if cst.kind == CstKind::ParensGroup => {
      args = parse_ast_function_args(errors, cst);
    }
    Some(other) => {
      errors.push(AstError::ErrGeneric(
        other.try_span().unwrap_or_default(),
        format!("Expected Paren Group, got {other:?}"),
      ));
    }
    None => {
      errors.push(AstError::ErrGeneric(
        cst.try_span().unwrap_or_default(),
        format!("Expected Paren Group, but no input."),
      ));
      out.kind = ItemKind::Function { args, ret_ty, statements };
      return;
    }
  };

  if matches!(it.next(), Some(&CstElem::FixedToken(MinusGreater, _))) {
    match it.next() {
      Some(CstElem::SubTree(ty_expr)) if ty_expr.kind == CstKind::TypeExpr => {
        ret_ty = parse_type_expr(errors, ty_expr);
      }
      other => {
        errors.push(AstError::ErrGeneric(
          out.span,
          format!("Expected return type, got {other:?}"),
        ));
      }
    }
  }

  match it.next() {
    Some(CstElem::SubTree(cst)) if cst.kind == CstKind::ValExpr => {
      statements = parse_value_expr_body(errors, cst);
    }
    Some(other) => {
      errors.push(AstError::ErrGeneric(
        other.try_span().unwrap_or_default(),
        format!("Expected function body, got {other:?}"),
      ));
    }
    None => {
      errors.push(AstError::ErrGeneric(
        cst.try_span().unwrap_or_default(),
        format!("Expected function body, but no input."),
      ));
    }
  };

  out.kind = ItemKind::Function { args, ret_ty, statements };

  for elem in it {
    println!("== {elem:?}");
  }
}

fn parse_type_expr(errors: &mut Vec<AstError>, cst: &Cst) -> TypeExpr {
  let mut out = TypeExpr::default();
  out.span = cst.try_span().unwrap_or_default();
  match cst.elements.as_slice() {
    [CstElem::Identifier(name, opt_span)] => {
      out.span = opt_span.unwrap_or_default();
      out.kind = Box::new(TypeExprKind::Simple(name.clone()));
    }
    other => {
      errors.push(AstError::ErrGeneric(
        out.span,
        format!("Expected single identifier as type expression, got {other:?}"),
      ));
    }
  };
  out
}

fn parse_value_expr(errors: &mut Vec<AstError>, cst: &Cst) -> ValueExpr {
  debug_assert_eq!(cst.kind, CstKind::ValExpr);
  //
  let span = cst.try_span().unwrap_or_default();
  let mut it = cst.elements.iter();
  match it.next() {
    Some(CstElem::FixedToken(OpBrace, _)) => ValueExpr {
      span,
      kind: Box::new(ValueExprKind::Block {
        statements: parse_value_expr_body(errors, cst),
      }),
    },
    Some(CstElem::FixedToken(KwFor, _)) => parse_value_expr_for(errors, cst),
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
    Some(CstElem::SubTree(cst)) if cst.kind == CstKind::ValExpr => {
      let left = parse_value_expr(errors, cst);
      match it.next() {
        Some(CstElem::SubTree(cst))
          if matches!(cst.kind, CstKind::OperatorInfix(op)) =>
        {
          let op = match cst.kind {
            CstKind::OperatorInfix(op) => BinOpKind::from(op),
            _ => unimplemented!(),
          };
          match it.next() {
            Some(CstElem::SubTree(cst)) if cst.kind == CstKind::ValExpr => {
              let right = parse_value_expr(errors, cst);
              ValueExpr {
                span,
                kind: Box::new(ValueExprKind::BinOp { left, op, right }),
              }
            }
            other => {
              errors.push(AstError::ErrGeneric(
                cst.try_span().unwrap_or_default(),
                format!("Expected value expression, got {other:?}"),
              ));
              let mut out = ValueExpr::default();
              out.span = cst.try_span().unwrap_or_default();
              out
            }
          }
        }
        Some(CstElem::SubTree(cst))
          if matches!(cst.kind, CstKind::OperatorPostfix(op)) =>
        {
          match cst.kind {
            CstKind::OperatorPostfix(
              PostfixOperator::PostfixRangeExclusive,
            ) => match it.next() {
              Some(CstElem::SubTree(cst)) if cst.kind == CstKind::ValExpr => {
                let op = BinOpKind::RangeExclusive;
                let right = parse_value_expr(errors, cst);
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
                errors.push(AstError::ErrGeneric(
                  cst.try_span().unwrap_or_default(),
                  format!("Expected value expression, got {other:?}"),
                ));
                let mut out = ValueExpr::default();
                out.span = cst.try_span().unwrap_or_default();
                out
              }
            },
            _ => unimplemented!(),
          }
        }
        other => {
          errors.push(AstError::ErrGeneric(
            cst.try_span().unwrap_or_default(),
            format!("Expected value expression, got {other:?}"),
          ));
          let mut out = ValueExpr::default();
          out.span = cst.try_span().unwrap_or_default();
          out
        }
      }
    }
    other => {
      errors.push(AstError::ErrGeneric(
        cst.try_span().unwrap_or_default(),
        format!("Expected value expression, got {other:?}"),
      ));
      let mut out = ValueExpr::default();
      out.span = cst.try_span().unwrap_or_default();
      out
    }
  }
}

fn parse_value_expr_for(errors: &mut Vec<AstError>, cst: &Cst) -> ValueExpr {
  debug_assert_eq!(cst.kind, CstKind::ValExpr);
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

  // KwFor, already checked by caller and also debug asserted for.
  let _ = it.next();
  match it.next() {
    Some(CstElem::SubTree(cst)) if cst.kind == CstKind::Pattern => {
      step_var = parse_pattern(errors, cst);
    }
    other => {
      errors.push(AstError::ErrGeneric(
        cst.try_span().unwrap_or_default(),
        format!("Expected pattern, got {other:?}"),
      ));
    }
  }
  match it.next() {
    Some(CstElem::FixedToken(KwIn, _)) => {}
    other => {
      errors.push(AstError::ErrGeneric(
        cst.try_span().unwrap_or_default(),
        format!("Expected pattern, got {other:?}"),
      ));
    }
  }
  match it.next() {
    Some(CstElem::SubTree(cst)) if cst.kind == CstKind::ValExpr => {
      range = parse_value_expr(errors, cst);
    }
    other => {
      errors.push(AstError::ErrGeneric(
        cst.try_span().unwrap_or_default(),
        format!("Expected range expression, got {other:?}"),
      ));
    }
  }
  match it.next() {
    Some(CstElem::SubTree(cst)) if cst.kind == CstKind::ValExpr => {
      statements = parse_value_expr_body(errors, cst);
    }
    other => {
      errors.push(AstError::ErrGeneric(
        cst.try_span().unwrap_or_default(),
        format!("Expected range expression, got {other:?}"),
      ));
    }
  }

  for i in it {
    dbg!(&i);
  }

  out.kind =
    Box::new(ValueExprKind::For { label, step_var, range, statements });
  out
}

/// Returns the statements of a cst holding a ValExpr body (`{ }`).
///
/// The return value is a vec so that this can be shared between an "actual"
/// body as well as with the other expression forms that have expression blocks.
fn parse_value_expr_body(
  errors: &mut Vec<AstError>, cst: &Cst,
) -> Vec<Statement> {
  debug_assert_eq!(cst.kind, CstKind::ValExpr);
  debug_assert_eq!(
    cst.elements.first().unwrap().fixed_token().unwrap().0,
    OpBrace
  );
  //
  let mut it = cst.elements.iter();
  let mut statements = Vec::new();

  // OpBrace, already checked by caller and also debug asserted for.
  let _ = it.next();
  loop {
    match it.next() {
      None => {
        errors.push(AstError::ErrGeneric(
          cst.try_span().unwrap_or_default(),
          format!("Expected Statement or Close Brace, but no input"),
        ));
        break;
      }
      Some(CstElem::FixedToken(ClBrace, _span)) => {
        break;
      }
      Some(CstElem::SubTree(cst)) if cst.kind == CstKind::Statement => {
        statements.push(parse_statement(errors, cst));
      }
      other => {
        errors.push(AstError::ErrGeneric(
          cst.try_span().unwrap_or_default(),
          format!("Expected statement, got {other:?}"),
        ));
      }
    }
  }

  statements
}

fn parse_statement(errors: &mut Vec<AstError>, cst: &Cst) -> Statement {
  debug_assert_eq!(cst.kind, CstKind::Statement);
  //
  match cst.elements.first() {
    Some(CstElem::FixedToken(KwLet, _)) => parse_statement_let(errors, cst),
    Some(CstElem::SubTree(cst)) if cst.kind == CstKind::ValExpr => {
      let x = parse_value_expr(errors, cst);
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

fn parse_statement_let(errors: &mut Vec<AstError>, cst: &Cst) -> Statement {
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

  // KwLet, already checked by caller and also debug asserted for.
  let _ = it.next();

  match it.next() {
    Some(CstElem::SubTree(cst)) if cst.kind == CstKind::Pattern => {
      pattern = parse_pattern(errors, cst);
    }
    None => {
      errors.push(AstError::ErrGeneric(
        cst.try_span().unwrap_or_default(),
        format!("Expected Pattern, but no input"),
      ));
      return out;
    }
    Some(other) => {
      errors.push(AstError::ErrGeneric(
        other.try_span().unwrap_or_default(),
        format!("Expected pattern, got {other:?}"),
      ));
    }
  }

  if matches!(it.peek(), Some(CstElem::FixedToken(Colon, _))) {
    let _ = it.next();
    match it.next() {
      Some(CstElem::SubTree(cst)) if cst.kind == CstKind::TypeExpr => {
        type_decl = Some(parse_type_expr(errors, cst));
      }
      None => {
        errors.push(AstError::ErrGeneric(
          cst.try_span().unwrap_or_default(),
          format!("Expected Type Expr, but no input"),
        ));
        return out;
      }
      Some(other) => {
        errors.push(AstError::ErrGeneric(
          other.try_span().unwrap_or_default(),
          format!("Expected Type Expr, got {other:?}"),
        ));
      }
    }
  }

  if matches!(it.peek(), Some(CstElem::FixedToken(Equal, _))) {
    let _ = it.next();
    match it.next() {
      Some(CstElem::SubTree(cst)) if cst.kind == CstKind::ValExpr => {
        initializer = Some(parse_value_expr(errors, cst));
      }
      None => {
        errors.push(AstError::ErrGeneric(
          cst.try_span().unwrap_or_default(),
          format!("Expected Value Expr, but no input"),
        ));
        return out;
      }
      Some(other) => {
        errors.push(AstError::ErrGeneric(
          other.try_span().unwrap_or_default(),
          format!("Expected Value Expr, got {other:?}"),
        ));
      }
    }
  }

  match it.next() {
    Some(CstElem::FixedToken(Semicolon, _)) => (),
    other => {
      errors.push(AstError::ErrGeneric(
        cst.try_span().unwrap_or_default(),
        format!("Expected `=`, got {other:?}"),
      ));
      return out;
    }
  }

  for i in it {
    println!("LET!! {i:?}");
  }

  out.kind = Box::new(StatementKind::Let { pattern, type_decl, initializer });
  out
}

fn parse_ast_function_args(
  errors: &mut Vec<AstError>, cst: &Cst,
) -> Vec<FunctionArg> {
  debug_assert_eq!(cst.kind, CstKind::ParensGroup);
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
      None => {
        errors.push(AstError::ErrGeneric(
          cst.try_span().unwrap_or_default(),
          format!("Expected Close Paren."),
        ));
        return out;
      }
      Some(CstElem::FixedToken(ClParen, _span)) => {
        debug_assert!(it.next().is_none());
        return out;
      }
      Some(CstElem::SubTree(pat_tree)) if pat_tree.kind == CstKind::Pattern => {
        let mut pattern = parse_pattern(errors, pat_tree);
        let mut type_decl = TypeExpr::default();
        match it.next() {
          Some(CstElem::FixedToken(Colon, _span)) => {}
          other => {
            errors.push(AstError::ErrGeneric(
              pat_tree.try_span().unwrap_or_default(),
              format!("Expected `:`, got {other:?}"),
            ));
          }
        }
        match it.next() {
          Some(CstElem::SubTree(ty_tree))
            if ty_tree.kind == CstKind::TypeExpr =>
          {
            type_decl = parse_type_expr(errors, ty_tree);
          }
          other => {
            errors.push(AstError::ErrGeneric(
              pat_tree.try_span().unwrap_or_default(),
              format!("Expected Type Expression, got {other:?}"),
            ));
          }
        }
        out.push(FunctionArg { pattern, type_decl });
      }
      Some(other) => {
        errors.push(AstError::ErrGeneric(
          other.try_span().unwrap_or_default(),
          format!("Expected Pattern, got {other:?}"),
        ));
      }
    }
  }
}

fn parse_pattern(errors: &mut Vec<AstError>, cst: &Cst) -> Pattern {
  debug_assert_eq!(cst.kind, CstKind::Pattern, "{cst}");
  //
  match cst.elements.as_slice() {
    [CstElem::Identifier(name, opt_span)] => Pattern {
      span: opt_span.unwrap_or_default(),
      kind: PatternKind::Simple(name.clone()),
    },
    other => {
      errors.push(AstError::ErrGeneric(
        cst.try_span().unwrap_or_default(),
        format!("Expected single identifier as pattern, got {other:?}"),
      ));
      Pattern {
        span: cst.try_span().unwrap_or_default(),
        kind: PatternKind::ErrPatternKind,
      }
    }
  }
}
