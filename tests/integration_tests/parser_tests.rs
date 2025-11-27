use chumsky::inspector::SimpleState;
use chumsky::prelude::*;
use str_id::StrID;
use yagbas::*;

#[allow(unused_macros)]
macro_rules! do_parse {
  ($parser:expr, $src: expr) => {{
    let source = $src;
    let file_id: FileID = fake_file_id(1);
    let mut parser_state = SimpleState(YagParserState { source, file_id });
    let tokens = tokens_of(source);
    let (trees, tree_errors) = trees_of(&tokens);
    assert!(tree_errors.is_empty());
    let (opt_output, output_errors) = $parser
      .parse_with_state(make_yag_parser_input(&trees), &mut parser_state)
      .into_output_errors();
    for error in &output_errors {
      println!("ERROR: {error:?}");
    }
    assert!(output_errors.is_empty());
    opt_output.unwrap()
  }};
}

fn span32(start: u32, end: u32) -> Span32 {
  Span32 { start, end, context: () }
}

fn str_id(str: &str) -> StrID {
  StrID::from(str)
}

fn fake_file_id(u: usize) -> FileID {
  if u == 0 {
    panic!()
  }
  unsafe { core::mem::transmute(u) }
}

fn mk_ident(span: Span32, ident: &str) -> Expr {
  Expr {
    span,
    kind: Box::new(ExprKind::Ident(ExprIdent { ident: str_id(ident) })),
  }
}
fn mk_num_lit(span: Span32, lit: &str) -> Expr {
  Expr {
    span,
    kind: Box::new(ExprKind::NumLit(ExprNumLit { lit: str_id(lit) })),
  }
}
fn mk_bool(span: Span32, b: bool) -> Expr {
  Expr { span, kind: Box::new(ExprKind::Bool(b)) }
}

#[test]
fn test_ident_p() {
  assert_eq!(do_parse!(ident_p(), "abc"), str_id("abc"));
  assert_eq!(do_parse!(ident_p(), "foo"), str_id("foo"));
  assert_eq!(do_parse!(ident_p(), "rusty"), str_id("rusty"));
  assert_eq!(do_parse!(ident_p(), "_"), str_id("_"));
}

#[test]
fn test_num_lit_p() {
  assert_eq!(do_parse!(num_lit_p(), "1"), str_id("1"));
  assert_eq!(do_parse!(num_lit_p(), "$FF"), str_id("$FF"));
  assert_eq!(do_parse!(num_lit_p(), "%111"), str_id("%111"));
  assert_eq!(do_parse!(num_lit_p(), "12_34"), str_id("12_34"));
}

#[test]
fn test_bool_p() {
  assert_eq!(do_parse!(bool_p(), "true"), true);
  assert_eq!(do_parse!(bool_p(), "false"), false);
}

#[test]
fn test_expr_p_ident() {
  assert_eq!(do_parse!(expr_p(), "a"), mk_ident(span32(0, 1), "a"));
}

#[test]
fn test_expr_p_num_lit() {
  assert_eq!(do_parse!(expr_p(), "123"), mk_num_lit(span32(0, 3), "123"));
}

#[test]
fn test_expr_p_bool() {
  assert_eq!(do_parse!(expr_p(), "true"), mk_bool(span32(0, 4), true));
  assert_eq!(do_parse!(expr_p(), "false"), mk_bool(span32(0, 5), false));
}

#[test]
fn test_expr_p_list() {
  assert_eq!(
    do_parse!(expr_p(), "[true, false]"),
    Expr {
      span: span32(0, 13),
      kind: Box::new(ExprKind::List(ExprList {
        elements: vec![
          Expr { span: span32(1, 5), kind: Box::new(ExprKind::Bool(true)) },
          Expr { span: span32(7, 12), kind: Box::new(ExprKind::Bool(false)) }
        ]
      }))
    }
  );
}

#[test]
fn test_expr_p_block() {
  assert_eq!(
    do_parse!(expr_p(), "{ true; false }"),
    Expr {
      span: span32(0, 15),
      kind: Box::new(ExprKind::Block(ExprBlock {
        body: vec![
          Statement {
            attribues: None,
            span: span32(2, 6),
            kind: Box::new(StatementKind::Expr(Expr {
              span: span32(2, 6),
              kind: Box::new(ExprKind::Bool(true))
            }))
          },
          Statement {
            attribues: None,
            span: span32(8, 13),
            kind: Box::new(StatementKind::Expr(Expr {
              span: span32(8, 13),
              kind: Box::new(ExprKind::Bool(false))
            }))
          }
        ]
      }))
    }
  );
}

#[test]
fn test_expr_p_call() {
  assert_eq!(
    do_parse!(expr_p(), "sqrt(123)"),
    Expr {
      span: span32(0, 9),
      kind: Box::new(ExprKind::Call(ExprCall {
        target: str_id("sqrt"),
        target_span: span32(0, 4),
        args: vec![Expr {
          span: span32(5, 8),
          kind: Box::new(ExprKind::NumLit(ExprNumLit { lit: str_id("123") }))
        }]
      }))
    }
  );
}

#[test]
fn test_expr_p_macro() {
  assert_eq!(
    do_parse!(expr_p(), "do_it!(123)"),
    Expr {
      span: span32(0, 11),
      kind: Box::new(ExprKind::Macro(ExprMacro {
        target: str_id("do_it"),
        target_span: span32(0, 5),
        args: vec![Expr {
          span: span32(7, 10),
          kind: Box::new(ExprKind::NumLit(ExprNumLit { lit: str_id("123") }))
        }]
      }))
    }
  );
}

#[test]
fn test_expr_p_struct_lit() {
  assert_eq!(
    do_parse!(expr_p(), "LcdCtrl { enabled }"),
    Expr {
      span: span32(0, 19),
      kind: Box::new(ExprKind::StructLit(ExprStructLit {
        ty: str_id("LcdCtrl"),
        ty_span: span32(0, 7),
        args: vec![Expr {
          span: span32(10, 17),
          kind: Box::new(ExprKind::Ident(ExprIdent {
            ident: str_id("enabled")
          }))
        }]
      }))
    }
  );
}

#[test]
fn test_expr_p_break() {
  assert_eq!(
    do_parse!(expr_p(), "break"),
    Expr {
      span: span32(0, 5),
      kind: Box::new(ExprKind::Break(ExprBreak { target: None, value: None }))
    }
  );
  assert_eq!(
    do_parse!(expr_p(), "break 'abc"),
    Expr {
      span: span32(0, 10),
      kind: Box::new(ExprKind::Break(ExprBreak {
        target: Some((str_id("abc"), span32(7, 10))),
        value: None
      }))
    }
  );
  assert_eq!(
    do_parse!(expr_p(), "break 2"),
    Expr {
      span: span32(0, 7),
      kind: Box::new(ExprKind::Break(ExprBreak {
        target: None,
        value: Some(Expr {
          span: span32(6, 7),
          kind: Box::new(ExprKind::NumLit(ExprNumLit { lit: str_id("2") }))
        })
      }))
    }
  );
  assert_eq!(
    do_parse!(expr_p(), "break 'abc 2"),
    Expr {
      span: span32(0, 12),
      kind: Box::new(ExprKind::Break(ExprBreak {
        target: Some((str_id("abc"), span32(7, 10))),
        value: Some(Expr {
          span: span32(11, 12),
          kind: Box::new(ExprKind::NumLit(ExprNumLit { lit: str_id("2") }))
        })
      }))
    }
  );
}

#[test]
fn test_expr_p_continue() {
  assert_eq!(
    do_parse!(expr_p(), "continue"),
    Expr {
      span: span32(0, 8),
      kind: Box::new(ExprKind::Continue(ExprContinue { target: None }))
    }
  );
  assert_eq!(
    do_parse!(expr_p(), "continue 'abc"),
    Expr {
      span: span32(0, 13),
      kind: Box::new(ExprKind::Continue(ExprContinue {
        target: Some((str_id("abc"), span32(10, 13))),
      }))
    }
  );
}

#[test]
fn test_expr_p_return() {
  assert_eq!(
    do_parse!(expr_p(), "return"),
    Expr {
      span: span32(0, 6),
      kind: Box::new(ExprKind::Return(ExprReturn { value: None }))
    }
  );
  assert_eq!(
    do_parse!(expr_p(), "return 2"),
    Expr {
      span: span32(0, 8),
      kind: Box::new(ExprKind::Return(ExprReturn {
        value: Some(Expr {
          span: span32(7, 8),
          kind: Box::new(ExprKind::NumLit(ExprNumLit { lit: str_id("2") }))
        })
      }))
    }
  );
}

#[test]
fn test_expr_p_add() {
  assert_eq!(
    do_parse!(expr_p(), "2+3"),
    Expr {
      span: span32(0, 3),
      kind: Box::new(ExprKind::BinOp(ExprBinOp {
        lhs: Expr {
          span: span32(0, 1),
          kind: Box::new(ExprKind::NumLit(ExprNumLit { lit: str_id("2") }))
        },
        rhs: Expr {
          span: span32(2, 3),
          kind: Box::new(ExprKind::NumLit(ExprNumLit { lit: str_id("3") }))
        },
        kind: BinOpKind::Add,
      }))
    }
  );

  assert_eq!(
    do_parse!(expr_p(), "2+3+4"),
    Expr {
      span: span32(0, 5),
      kind: Box::new(ExprKind::BinOp(ExprBinOp {
        lhs: Expr {
          span: span32(0, 3),
          kind: Box::new(ExprKind::BinOp(ExprBinOp {
            lhs: Expr {
              span: span32(0, 1),
              kind: Box::new(ExprKind::NumLit(ExprNumLit { lit: str_id("2") }))
            },
            rhs: Expr {
              span: span32(2, 3),
              kind: Box::new(ExprKind::NumLit(ExprNumLit { lit: str_id("3") }))
            },
            kind: BinOpKind::Add,
          }))
        },
        rhs: Expr {
          span: span32(4, 5),
          kind: Box::new(ExprKind::NumLit(ExprNumLit { lit: str_id("4") }))
        },
        kind: BinOpKind::Add,
      }))
    }
  );
}

// todo pratt parsing test cases

#[test]
fn test_item_p_bitbag() {
  assert_eq!(
    do_parse!(item_p(), "bitbag Test { zero: 0, one: 1 }"),
    AstItem {
      attributes: Vec::new(),
      file_id: fake_file_id(1),
      span: span32(0, 31),
      name: str_id("Test"),
      name_span: span32(7, 11),
      kind: AstItemKind::Bitbag(AstBitbag {
        fields: vec![
          AstBitbagFieldDef {
            attributes: vec![],
            span: span32(14, 21),
            name: str_id("zero"),
            name_span: span32(14, 18),
            bit: Expr {
              span: span32(20, 21),
              kind: Box::new(ExprKind::NumLit(ExprNumLit { lit: str_id("0") })),
            },
            bit_span: span32(20, 21)
          },
          AstBitbagFieldDef {
            attributes: vec![],
            span: span32(23, 29),
            name: str_id("one"),
            name_span: span32(23, 26),
            bit: Expr {
              span: span32(28, 29),
              kind: Box::new(ExprKind::NumLit(ExprNumLit { lit: str_id("1") })),
            },
            bit_span: span32(28, 29),
          },
        ]
      })
    }
  );
}
