#![allow(clippy::ptr_arg)]
#![allow(unused_variables)]

use crate::ValueExprId;
use crate::tokenizer::TokenKind::Comma;
use crate::{
  Span,
  ast::{
    FunctionArg, FunctionData, Item, ItemId, ItemKind, Module, Statement,
    StatementKind, TypeExpr, TypeExprKind, ValueExpr, ValueExprKind,
    parser::AstParser,
  },
  cst::{
    Cst, CstElem,
    CstKind::{self},
  },
  operators::{BinOpKind, PostOp, PrefOp, UnOpKind},
  path_id::PathId,
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

/// * `($p:expr, $it:expr, $eoi_span:expr)`
macro_rules! basic_type_expr {
  ($p:expr, $it:expr, $eoi_span:expr) => {{
    match $it.next() {
      Some(CstElem::SubTree(cst)) if cst.kind == CstKind::TypeExpr => {
        Some(parse_type_expr($p, cst))
      }
      Some(other) => {
        $p.error_at(
          other.try_span().unwrap_or_default(),
          format!("Expected Type Expression, got: {other:?}"),
        );
        None
      }
      None => {
        $p.error_at(
          $eoi_span,
          format!("Expected Type Expression, got EndOfGrouping"),
        );
        None
      }
    }
  }};
}

/// * `($p:expr, $it:expr, $eoi_span:expr)`
macro_rules! basic_value_expr {
  ($p:expr, $it:expr, $eoi_span:expr) => {{
    match $it.next() {
      Some(CstElem::SubTree(cst)) if cst.kind == CstKind::ValueExpr => {
        Some(parse_value_expr($p, cst))
      }
      Some(other) => {
        $p.error_at(
          other.try_span().unwrap_or_default(),
          format!("Expected Value Expression, got: {other:?}"),
        );
        None
      }
      None => {
        $p.error_at(
          $eoi_span,
          format!("Expected Value Expression, got EndOfGrouping"),
        );
        None
      }
    }
  }};
}

/// * `($p:expr, $it:expr, $eoi_span:expr)`
macro_rules! basic_identifier {
  ($p:expr, $it:expr, $eoi_span:expr) => {{
    match $it.next() {
      Some(CstElem::Identifier(name, opt_span)) => {
        Some((name, opt_span.unwrap_or_default()))
      }
      Some(other) => {
        $p.error_at(
          other.try_span().unwrap_or_default(),
          format!("Expected Identifier, got: {other:?}"),
        );
        None
      }
      None => {
        $p.error_at(
          $eoi_span,
          format!("Expected Identifier, got EndOfGrouping"),
        );
        None
      }
    }
  }};
}

/// * `($p:expr, $it:expr, $eoi_span:expr)`
macro_rules! basic_value_expr_body {
  ($p:expr, $it:expr, $eoi_span:expr) => {{
    match $it.next() {
      Some(CstElem::SubTree(cst)) if cst.kind == CstKind::ValueExpr => {
        Some(parse_value_expr_body($p, cst))
      }
      Some(other) => {
        $p.error_at(
          other.try_span().unwrap_or_default(),
          format!("Expected Body Expression, got: {other:?}"),
        );
        None
      }
      None => {
        $p.error_at(
          $eoi_span,
          format!("Expected Body Expression, got EndOfGrouping"),
        );
        None
      }
    }
  }};
}

pub fn parse_ast_module(
  p: &mut AstParser, file_origin: PathId, cst: &Cst,
) -> Module {
  debug_assert_eq!(cst.kind, CstKind::Module);
  //
  let mut out = Module::default();
  out.file_origin = file_origin;
  for elem in &cst.elements {
    match elem {
      CstElem::SubTree(cst) if cst.kind == CstKind::Item => {
        let item = parse_ast_item(p, file_origin, cst);
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

fn parse_ast_item(p: &mut AstParser, file_origin: PathId, cst: &Cst) -> Item {
  debug_assert_eq!(cst.kind, CstKind::Item);
  //
  let mut out = Item {
    file_origin,
    span: cst.try_span().unwrap_or_default(),
    id: ItemId::new(),
    name: String::new(),
    name_span: Span::default(),
    kind: ItemKind::ErrItemKind,
  };
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

  basic_fixed_token!(p, it, out.span, KwFn);

  if let Some((name, name_span)) = basic_identifier!(p, it, out.span) {
    out.name = name.clone();
    out.name_span = name_span;
  }

  let args = match it.next() {
    Some(CstElem::SubTree(cst)) if cst.kind == CstKind::Parens => {
      parse_ast_function_args(p, cst)
    }
    other => {
      p.error_at(
        cst.try_span().unwrap_or_default(),
        format!("Expected Parens Group: {other:?}"),
      );
      Vec::default()
    }
  };

  let opt_ret_tyx =
    if matches!(it.peek(), Some(&CstElem::FixedToken(MinusGreater, _))) {
      basic_fixed_token!(p, it, out.span, MinusGreater);
      basic_type_expr!(p, it, out.span)
    } else {
      None
    };

  let body = basic_value_expr_body!(p, it, out.span).unwrap_or_default();

  out.kind =
    ItemKind::Function(Box::new(FunctionData { args, opt_ret_tyx, body }));

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

  basic_fixed_token!(p, it, out.span, KwConst);

  if let Some((name, name_span)) = basic_identifier!(p, it, out.span) {
    out.name = name.clone();
    out.name_span = name_span;
  }

  basic_fixed_token!(p, it, out.span, Colon);

  let type_decl = basic_type_expr!(p, it, out.span).unwrap_or_default();

  basic_fixed_token!(p, it, out.span, Equal);

  let value_decl = basic_value_expr!(p, it, out.span).unwrap_or_default();

  basic_fixed_token!(p, it, out.span, Semicolon);

  out.kind = ItemKind::Constant { type_decl, value_decl };

  for i in it {
    dbg!(&i);
  }
}

fn parse_ast_static(p: &mut AstParser, cst: &Cst, out: &mut Item) {
  debug_assert_eq!(
    cst.elements.first().unwrap().fixed_token().unwrap().0,
    KwStatic
  );
  //
  let mut it = cst.elements.iter().peekable();

  basic_fixed_token!(p, it, out.span, KwStatic);

  match it.next() {
    Some(CstElem::FixedToken(KwMmio, _)) => {
      basic_fixed_token!(p, it, out.span, OpParen);

      let location = basic_value_expr!(p, it, out.span).unwrap_or_default();

      basic_fixed_token!(p, it, out.span, ClParen);

      if let Some((name, name_span)) = basic_identifier!(p, it, out.span) {
        out.name = name.clone();
        out.name_span = name_span;
      }

      basic_fixed_token!(p, it, out.span, Colon);

      let type_decl = basic_type_expr!(p, it, out.span).unwrap_or_default();

      out.kind = ItemKind::StaticMmio { location, type_decl };
    }
    Some(CstElem::FixedToken(KwRam, _)) => {
      if let Some((name, name_span)) = basic_identifier!(p, it, out.span) {
        out.name = name.clone();
        out.name_span = name_span;
      }

      basic_fixed_token!(p, it, out.span, Colon);

      let type_decl = basic_type_expr!(p, it, out.span).unwrap_or_default();

      basic_fixed_token!(p, it, out.span, Equal);

      let init = basic_value_expr!(p, it, out.span).unwrap_or_default();

      out.kind = ItemKind::StaticRam { type_decl, init };
    }
    Some(CstElem::FixedToken(KwRom, _)) => {
      if let Some((name, name_span)) = basic_identifier!(p, it, out.span) {
        out.name = name.clone();
        out.name_span = name_span;
      }

      basic_fixed_token!(p, it, out.span, Colon);

      let type_decl = basic_type_expr!(p, it, out.span).unwrap_or_default();

      basic_fixed_token!(p, it, out.span, Equal);

      let data = basic_value_expr!(p, it, out.span).unwrap_or_default();

      out.kind = ItemKind::StaticRom { type_decl, data };
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
      out.kind = Box::new(TypeExprKind::Identifier(name.clone()));
    }
    Some(CstElem::FixedToken(OpBracket, _)) => {
      let elem_ty = basic_type_expr!(p, it, out.span).unwrap_or_default();

      basic_fixed_token!(p, it, out.span, Semicolon);

      let elem_count = basic_value_expr!(p, it, out.span).unwrap_or_default();

      basic_fixed_token!(p, it, out.span, ClBracket);

      out.kind =
        Box::new(TypeExprKind::Array { elem_tyx: elem_ty, elem_count });
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
  debug_assert_eq!(cst.kind, CstKind::ValueExpr);
  //
  let span = cst.try_span().unwrap_or_default();
  let mut it = cst.elements.iter();
  match it.next() {
    Some(CstElem::FixedToken(OpBrace, _)) => parse_value_expr_body(p, cst),
    Some(CstElem::FixedToken(KwFor, _)) => parse_value_expr_for(p, cst),
    Some(CstElem::FixedToken(KwLoop, _)) => parse_value_expr_loop(p, cst),
    Some(CstElem::FixedToken(KwIf, _)) => parse_value_expr_if(p, cst),
    Some(CstElem::Identifier(string, opt_span)) => ValueExpr {
      span: opt_span.unwrap_or_default(),
      id: ValueExprId::new(),
      kind: Box::new(ValueExprKind::Identifier(string.clone())),
    },
    Some(CstElem::LitNumber(string, opt_span)) => ValueExpr {
      span: opt_span.unwrap_or_default(),
      id: ValueExprId::new(),
      kind: Box::new(ValueExprKind::LiteralNumber(string.clone())),
    },
    Some(CstElem::LitString(string, opt_span)) => ValueExpr {
      span: opt_span.unwrap_or_default(),
      id: ValueExprId::new(),
      kind: Box::new(ValueExprKind::LiteralString(string.clone())),
    },
    Some(CstElem::SubTree(cst)) => match cst.kind {
      CstKind::ValueExpr => {
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
              Some(CstElem::SubTree(cst)) if cst.kind == CstKind::ValueExpr => {
                let right = parse_value_expr(p, cst);
                ValueExpr {
                  span,
                  id: ValueExprId::new(),
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
                    if cst.kind == CstKind::ValueExpr =>
                  {
                    let op = BinOpKind::RangeExclusive;
                    let right = parse_value_expr(p, cst);
                    ValueExpr {
                      span,
                      id: ValueExprId::new(),
                      kind: Box::new(ValueExprKind::BinOp { left, op, right }),
                    }
                  }
                  None => {
                    let op = UnOpKind::PostfixRangeExclusive;
                    ValueExpr {
                      span,
                      id: ValueExprId::new(),
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
                  if cst.kind == CstKind::ValueExpr =>
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
                    id: ValueExprId::new(),
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
          if matches!(cst.kind, CstKind::ValueExpr) =>
        {
          ValueExpr {
            span,
            id: ValueExprId::new(),
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
          if matches!(cst.kind, CstKind::ValueExpr) =>
        {
          ValueExpr {
            span,
            id: ValueExprId::new(),
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
          id: ValueExprId::new(),
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
  debug_assert_eq!(cst.kind, CstKind::ValueExpr);
  debug_assert_eq!(
    cst.elements.first().unwrap().fixed_token().unwrap().0,
    KwFor
  );
  //
  let mut it = cst.elements.iter();
  let label = None;
  let mut out = ValueExpr::default();
  out.span = cst.try_span().unwrap_or_default();

  basic_fixed_token!(p, it, out.span, KwFor);

  let step_var = basic_value_expr!(p, it, out.span).unwrap_or_default();

  basic_fixed_token!(p, it, out.span, KwIn);

  let range = basic_value_expr!(p, it, out.span).unwrap_or_default();

  let body = basic_value_expr_body!(p, it, out.span).unwrap_or_default();

  for i in it {
    dbg!(&i);
  }

  out.kind = Box::new(ValueExprKind::For { label, step_var, range, body });
  out
}

fn parse_value_expr_loop(p: &mut AstParser, cst: &Cst) -> ValueExpr {
  debug_assert_eq!(cst.kind, CstKind::ValueExpr);
  debug_assert_eq!(
    cst.elements.first().unwrap().fixed_token().unwrap().0,
    KwLoop
  );
  //
  let mut it = cst.elements.iter();
  let mut label = None;
  let mut out = ValueExpr::default();
  out.span = cst.try_span().unwrap_or_default();

  basic_fixed_token!(p, it, out.span, KwLoop);

  let body = basic_value_expr_body!(p, it, out.span).unwrap_or_default();

  for i in it {
    dbg!(&i);
  }

  out.kind = Box::new(ValueExprKind::Loop { label, body });
  out
}

fn parse_value_expr_if(p: &mut AstParser, cst: &Cst) -> ValueExpr {
  debug_assert_eq!(cst.kind, CstKind::ValueExpr);
  debug_assert_eq!(
    cst.elements.first().unwrap().fixed_token().unwrap().0,
    KwIf
  );
  //
  let mut it = cst.elements.iter().peekable();
  let mut out = ValueExpr::default();
  out.span = cst.try_span().unwrap_or_default();

  basic_fixed_token!(p, it, out.span, KwIf);

  let condition = basic_value_expr!(p, it, out.span).unwrap_or_default();

  let true_body = basic_value_expr_body!(p, it, out.span).unwrap_or_default();

  let opt_false_body =
    if matches!(it.peek(), Some(CstElem::FixedToken(KwElse, _))) {
      basic_fixed_token!(p, it, out.span, KwElse);

      Some(basic_value_expr_body!(p, it, out.span).unwrap_or_default())
    } else {
      None
    };

  for i in it {
    dbg!(&i);
  }

  out.kind =
    Box::new(ValueExprKind::If { condition, true_body, opt_false_body });
  out
}

/// Returns the statements of a cst holding a ValExpr body (`{ }`).
///
/// The return value is a vec so that this can be shared between an "actual"
/// body as well as with the other expression forms that have expression blocks.
fn parse_value_expr_body(p: &mut AstParser, cst: &Cst) -> ValueExpr {
  debug_assert_eq!(cst.kind, CstKind::ValueExpr);
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

  ValueExpr {
    id: ValueExprId::new(),
    span: cst.try_span().unwrap_or_default(),
    kind: Box::new(ValueExprKind::Block { statements }),
  }
}

fn parse_statement(p: &mut AstParser, cst: &Cst) -> Statement {
  debug_assert_eq!(cst.kind, CstKind::Statement);
  //
  match cst.elements.first() {
    Some(CstElem::FixedToken(KwLet, _)) => parse_statement_let(p, cst),
    Some(CstElem::SubTree(cst)) if cst.kind == CstKind::ValueExpr => {
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

  basic_fixed_token!(p, it, out.span, KwLet);

  let var = basic_value_expr!(p, it, out.span).unwrap_or_default();

  let type_decl = if matches!(it.peek(), Some(CstElem::FixedToken(Colon, _))) {
    basic_fixed_token!(p, it, out.span, Colon);
    basic_type_expr!(p, it, out.span)
  } else {
    None
  };

  let initializer = if matches!(it.peek(), Some(CstElem::FixedToken(Equal, _)))
  {
    basic_fixed_token!(p, it, out.span, Equal);
    basic_value_expr!(p, it, out.span)
  } else {
    None
  };

  basic_fixed_token!(p, it, out.span, Semicolon);

  for i in it {
    dbg!(&i);
  }

  out.kind = Box::new(StatementKind::Let { var, type_decl, initializer });
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
      Some(CstElem::FixedToken(Comma, _span)) => {
        continue;
      }
      Some(CstElem::FixedToken(ClParen, _span)) => {
        debug_assert!(it.next().is_none());
        return out;
      }
      Some(CstElem::SubTree(pat_tree))
        if pat_tree.kind == CstKind::IdentColonTypeExpr =>
      {
        let mut var = parse_value_expr(p, pat_tree);
        basic_fixed_token!(p, it, cst.try_span().unwrap_or_default(), Colon);
        TypeExpr::default();
        let type_decl =
          basic_type_expr!(p, it, cst.try_span().unwrap_or_default())
            .unwrap_or_default();
        out.push(FunctionArg { var, type_decl });
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
