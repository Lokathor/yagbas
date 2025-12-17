use chumsky::extra::State;
use chumsky::inspector::SimpleState;
use chumsky::{prelude::*, span};
use rayon::vec;
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
  Expr { span, kind: Box::new(ExprKind::Ident(str_id(ident))) }
}
fn mk_num_lit(span: Span32, lit: &str) -> Expr {
  Expr { span, kind: Box::new(ExprKind::NumLit(str_id(lit))) }
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
fn test_expr_p_list_empty() {
  assert_eq!(
    do_parse!(expr_p(), "[]"),
    Expr { span: span32(0, 2), kind: Box::new(ExprKind::List(vec![])) }
  );
}

#[test]
fn test_expr_p_list() {
  assert_eq!(
    do_parse!(expr_p(), "[true, false]"),
    Expr {
      span: span32(0, 13),
      kind: Box::new(ExprKind::List(vec![
        Expr { span: span32(1, 5), kind: Box::new(ExprKind::Bool(true)) },
        Expr { span: span32(7, 12), kind: Box::new(ExprKind::Bool(false)) }
      ]))
    }
  );
}

#[test]
fn test_expr_p_block_empty() {
  assert_eq!(
    do_parse!(expr_p(), "{}"),
    Expr {
      span: span32(0, 2),
      kind: Box::new(ExprKind::Block(StatementBody {
        body: vec![],
        last_expr: None
      }))
    }
  );
}

#[test]
fn test_expr_p_block() {
  assert_eq!(
    do_parse!(expr_p(), "{ a = true; false }"),
    Expr {
      span: span32(0, 19),
      kind: Box::new(ExprKind::Block(StatementBody {
        body: vec![Statement {
          span: span32(2, 10),
          attribues: None,
          kind: Box::new(StatementKind::Assign(
            Expr {
              span: span32(2, 3),
              kind: Box::new(ExprKind::Ident(str_id("a")))
            },
            Expr { span: span32(6, 10), kind: Box::new(ExprKind::Bool(true)) }
          ))
        }],
        last_expr: Some(Expr {
          span: span32(12, 17),
          kind: Box::new(ExprKind::Bool(false))
        })
      }))
    }
  );
  assert_eq!(
    do_parse!(expr_p(), "{ a = true; b = false; }"),
    Expr {
      span: span32(0, 24),
      kind: Box::new(ExprKind::Block(StatementBody {
        body: vec![
          Statement {
            span: span32(2, 10),
            attribues: None,
            kind: Box::new(StatementKind::Assign(
              Expr {
                span: span32(2, 3),
                kind: Box::new(ExprKind::Ident(str_id("a")))
              },
              Expr {
                span: span32(6, 10),
                kind: Box::new(ExprKind::Bool(true))
              }
            ))
          },
          Statement {
            span: span32(12, 21),
            attribues: None,
            kind: Box::new(StatementKind::Assign(
              Expr {
                span: span32(12, 13),
                kind: Box::new(ExprKind::Ident(str_id("b")))
              },
              Expr {
                span: span32(16, 21),
                kind: Box::new(ExprKind::Bool(false))
              }
            ))
          }
        ],
        last_expr: None,
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
      kind: Box::new(ExprKind::BinOp(ExprBinOp {
        lhs: Expr {
          span: span32(0, 4),
          kind: Box::new(ExprKind::Ident(str_id("sqrt")))
        },
        rhs: Expr {
          span: span32(4, 9),
          kind: Box::new(ExprKind::List(vec![Expr {
            span: span32(5, 8),
            kind: Box::new(ExprKind::NumLit(str_id("123")))
          }]))
        },
        kind: BinOpKind::Call
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
          kind: Box::new(ExprKind::NumLit(str_id("123")))
        }]
      }))
    }
  );
}

#[test]
fn test_expr_p_struct_lit_empty() {
  assert_eq!(
    do_parse!(expr_p(), "LcdCtrl {}"),
    Expr {
      span: span32(0, 10),
      kind: Box::new(ExprKind::StructLit(ExprStructLit {
        ty: str_id("LcdCtrl"),
        ty_span: span32(0, 7),
        args: vec![]
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
        args: vec![StructLitFieldInitKind::Activated(
          str_id("enabled"),
          span32(10, 17)
        )]
      }))
    }
  );
  assert_eq!(
    do_parse!(expr_p(), "LcdCtrl { enabled, }"),
    Expr {
      span: span32(0, 20),
      kind: Box::new(ExprKind::StructLit(ExprStructLit {
        ty: str_id("LcdCtrl"),
        ty_span: span32(0, 7),
        args: vec![StructLitFieldInitKind::Activated(
          str_id("enabled"),
          span32(10, 17)
        )]
      }))
    }
  );
  assert_eq!(
    do_parse!(expr_p(), "Palette { i0 = 1 }"),
    Expr {
      span: span32(0, 18),
      kind: Box::new(ExprKind::StructLit(ExprStructLit {
        ty: str_id("Palette"),
        ty_span: span32(0, 7),
        args: vec![StructLitFieldInitKind::Assign(
          str_id("i0"),
          span32(10, 12),
          Expr {
            span: span32(15, 16),
            kind: Box::new(ExprKind::NumLit(str_id("1")))
          }
        )]
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
          kind: Box::new(ExprKind::NumLit(str_id("2")))
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
          kind: Box::new(ExprKind::NumLit(str_id("2")))
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
          kind: Box::new(ExprKind::NumLit(str_id("2")))
        })
      }))
    }
  );
}

#[test]
fn test_expr_p_loop_empty() {
  assert_eq!(
    do_parse!(expr_p(), "loop {}"),
    Expr {
      span: span32(0, 7),
      kind: Box::new(ExprKind::Loop(ExprLoop {
        name: None,
        steps: StatementBody { body: vec![], last_expr: None }
      }))
    }
  );
}

#[test]
fn test_expr_p_loop() {
  assert_eq!(
    do_parse!(expr_p(), "loop { 1 }"),
    Expr {
      span: span32(0, 10),
      kind: Box::new(ExprKind::Loop(ExprLoop {
        name: None,
        steps: StatementBody {
          body: vec![],
          last_expr: Some(Expr {
            span: span32(7, 8),
            kind: Box::new(ExprKind::NumLit(str_id("1")))
          })
        }
      }))
    }
  );
}

#[test]
fn test_expr_p_loop_times_empty() {
  assert_eq!(
    do_parse!(expr_p(), "loop 3 times {}"),
    Expr {
      span: span32(0, 15),
      kind: Box::new(ExprKind::LoopTimes(ExprLoopTimes {
        name: None,
        times: str_id("3"),
        times_span: span32(5, 6),
        steps: StatementBody { body: vec![], last_expr: None }
      }))
    }
  );
  assert_eq!(
    do_parse!(expr_p(), "loop count times {}"),
    Expr {
      span: span32(0, 19),
      kind: Box::new(ExprKind::LoopTimes(ExprLoopTimes {
        name: None,
        times: str_id("count"),
        times_span: span32(5, 10),
        steps: StatementBody { body: vec![], last_expr: None }
      }))
    }
  );
}

#[test]
fn test_expr_p_if_empty() {
  assert_eq!(
    do_parse!(expr_p(), "if condition {}"),
    Expr {
      span: span32(0, 15),
      kind: Box::new(ExprKind::IfElse(ExprIfElse {
        condition: Expr {
          span: span32(3, 12),
          kind: Box::new(ExprKind::Ident(str_id("condition")))
        },
        if_: StatementBody { body: vec![], last_expr: None },
        else_: None,
      }))
    }
  );
}

#[test]
fn test_expr_p_if_else_empty() {
  assert_eq!(
    do_parse!(expr_p(), "if condition {} else {}"),
    Expr {
      span: span32(0, 23),
      kind: Box::new(ExprKind::IfElse(ExprIfElse {
        condition: Expr {
          span: span32(3, 12),
          kind: Box::new(ExprKind::Ident(str_id("condition")))
        },
        if_: StatementBody { body: vec![], last_expr: None },
        else_: Some(StatementBody { body: vec![], last_expr: None }),
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
          kind: Box::new(ExprKind::NumLit(str_id("2")))
        },
        rhs: Expr {
          span: span32(2, 3),
          kind: Box::new(ExprKind::NumLit(str_id("3")))
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
              kind: Box::new(ExprKind::NumLit(str_id("2")))
            },
            rhs: Expr {
              span: span32(2, 3),
              kind: Box::new(ExprKind::NumLit(str_id("3")))
            },
            kind: BinOpKind::Add,
          }))
        },
        rhs: Expr {
          span: span32(4, 5),
          kind: Box::new(ExprKind::NumLit(str_id("4")))
        },
        kind: BinOpKind::Add,
      }))
    }
  );
}

// todo many more pratt parsing test cases

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
              kind: Box::new(ExprKind::NumLit(str_id("0"))),
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
              kind: Box::new(ExprKind::NumLit(str_id("1"))),
            },
            bit_span: span32(28, 29),
          },
        ]
      })
    }
  );
}

#[test]
fn test_item_p_struct() {
  assert_eq!(
    do_parse!(item_p(), "struct Test { zero: u8, one: i8 }"),
    AstItem {
      attributes: Vec::new(),
      file_id: fake_file_id(1),
      span: span32(0, 33),
      name: str_id("Test"),
      name_span: span32(7, 11),
      kind: AstItemKind::Struct(AstStruct {
        fields: vec![
          AstStructFieldDef {
            attributes: vec![],
            span: span32(14, 22),
            name: str_id("zero"),
            name_span: span32(14, 18),
            ty: TypeName {
              span: span32(20, 22),
              kind: TypeNameKind::Ident(str_id("u8"))
            }
          },
          AstStructFieldDef {
            attributes: vec![],
            span: span32(24, 31),
            name: str_id("one"),
            name_span: span32(24, 27),
            ty: TypeName {
              span: span32(29, 31),
              kind: TypeNameKind::Ident(str_id("i8"))
            },
          },
        ]
      })
    }
  );
}

#[test]
fn test_item_p_const() {
  assert_eq!(
    do_parse!(item_p(), "const ONE: u8 = 1;"),
    AstItem {
      span: span32(0, 18),
      attributes: vec![],
      file_id: fake_file_id(1),
      name: str_id("ONE"),
      name_span: span32(6, 9),
      kind: AstItemKind::Const(AstConst {
        ty: TypeName {
          span: span32(11, 13),
          kind: TypeNameKind::Ident(str_id("u8"))
        },
        expr: Expr {
          span: span32(16, 17),
          kind: Box::new(ExprKind::NumLit(str_id("1")))
        }
      }),
    }
  )
}

#[test]
fn test_item_p_static() {
  assert_eq!(
    do_parse!(item_p(), "static rom DATA: i8 = 3;"),
    AstItem {
      span: span32(0, 24),
      attributes: vec![],
      file_id: fake_file_id(1),
      name: str_id("DATA"),
      name_span: span32(11, 15),
      kind: AstItemKind::Static(AstStatic {
        ty: TypeName {
          span: span32(17, 19),
          kind: TypeNameKind::Ident(str_id("i8"))
        },
        kind: AstStaticKind::Rom(Expr {
          span: span32(22, 23),
          kind: Box::new(ExprKind::NumLit(str_id("3")))
        }),
      }),
    }
  )
}

#[test]
fn test_item_p_function() {
  assert_eq!(
    do_parse!(item_p(), "fn foo() {}"),
    AstItem {
      span: span32(0, 11),
      attributes: vec![],
      file_id: fake_file_id(1),
      name: str_id("foo"),
      name_span: span32(3, 6),
      kind: AstItemKind::Function(AstFunction {
        args: vec![],
        return_info: None,
        body: StatementBody { body: vec![], last_expr: None }
      }),
    }
  )
}

#[test]
fn test_static_mmio_p() {
  assert_eq!(
    do_parse!(
      static_p(),
      "#[location($FE00)]
      static mmio OAM_RAM: [Obj; 40];"
    ),
    AstItem {
      attributes: vec![Expr {
        span: span32(2, 17),
        kind: Box::new(ExprKind::BinOp(ExprBinOp {
          lhs: Expr {
            span: span32(2, 10),
            kind: Box::new(ExprKind::Ident(str_id("location")))
          },
          rhs: Expr {
            span: span32(10, 17),
            kind: Box::new(ExprKind::List(vec![Expr {
              span: span32(11, 16),
              kind: Box::new(ExprKind::NumLit(str_id("$FE00")))
            }]))
          },
          kind: BinOpKind::Call
        }))
      }],
      file_id: fake_file_id(1),
      span: span32(0, 56),
      name: str_id("OAM_RAM"),
      name_span: span32(37, 44),
      kind: AstItemKind::Static(AstStatic {
        ty: TypeName {
          span: span32(46, 55),
          kind: TypeNameKind::ArrayNumLit(
            Box::new(TypeName {
              span: span32(47, 50),
              kind: TypeNameKind::Ident(str_id("Obj"))
            }),
            str_id("40"),
            span32(52, 54),
          )
        },
        kind: AstStaticKind::MemoryMappedIO,
      }),
    }
  )
}

#[test]
fn test_struct_p() {
  assert_eq!(
    do_parse!(
      struct_p(),
      "struct Obj {
        y: u8,
        x: u8,
        tile_id: u8,
        attrs: ObjAttrs,
      }"
    ),
    AstItem {
      span: span32(0, 96),
      attributes: vec![],
      file_id: fake_file_id(1),
      name: str_id("Obj"),
      name_span: span32(7, 10),
      kind: AstItemKind::Struct(AstStruct {
        fields: vec![
          AstStructFieldDef {
            span: span32(21, 26),
            attributes: vec![],
            name: str_id("y"),
            name_span: span32(21, 22),
            ty: TypeName {
              span: span32(24, 26),
              kind: TypeNameKind::Ident(str_id("u8"))
            }
          },
          AstStructFieldDef {
            span: span32(36, 41),
            attributes: vec![],
            name: str_id("x"),
            name_span: span32(36, 37),
            ty: TypeName {
              span: span32(39, 41),
              kind: TypeNameKind::Ident(str_id("u8"))
            }
          },
          AstStructFieldDef {
            span: span32(51, 62),
            attributes: vec![],
            name: str_id("tile_id"),
            name_span: span32(51, 58),
            ty: TypeName {
              span: span32(60, 62),
              kind: TypeNameKind::Ident(str_id("u8"))
            }
          },
          AstStructFieldDef {
            span: span32(72, 87),
            attributes: vec![],
            name: str_id("attrs"),
            name_span: span32(72, 77),
            ty: TypeName {
              span: span32(79, 87),
              kind: TypeNameKind::Ident(str_id("ObjAttrs"))
            }
          }
        ]
      }),
    }
  )
}

#[test]
fn test_statement_p_assign() {
  assert_eq!(
    do_parse!(statement_p(), "a = true"),
    Statement {
      attribues: None,
      span: span32(0, 8),
      kind: Box::new(StatementKind::Assign(
        Expr {
          span: span32(0, 1),
          kind: Box::new(ExprKind::Ident(str_id("a")))
        },
        Expr { span: span32(4, 8), kind: Box::new(ExprKind::Bool(true)) }
      ))
    }
  );
}
