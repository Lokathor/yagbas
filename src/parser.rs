use crate::Token::*;
use crate::TokenTree::*;
use crate::*;
use chumsky::text::ascii::keyword;
use chumsky::{input::MappedInput, prelude::*, recursive::Indirect};

#[allow(unused_macros)]
macro_rules! infix_maker {
  ($kind: path) => {
    |lhs, _op, rhs, extras| Expr {
      span: extras.span(),
      kind: Box::new(ExprKind::BinOp(lhs, $kind, rhs)),
    }
  };
}
#[allow(unused_macros)]
macro_rules! prefix_maker {
  ($kind: path) => {
    |_op, inner, extras| Expr {
      span: extras.span(),
      kind: Box::new(ExprKind::UnOp(inner, $kind)),
    }
  };
}

pub fn items_of<'src>(
  trees: &'src [(TokenTree, Span32)], yag_state: YagParserState,
) -> (Vec<AstItem>, Vec<Rich<'src, TokenTree, Span32>>) {
  let bad_item_recovery =
    via_parser(any().repeated().at_least(1).map_with(|_, ex| AstItem {
      attributes: Vec::new(),
      file_id: yag_state.file_id,
      span: ex.span(),
      kind: AstItemKind::AstItemKindError,
    }));

  let parser =
    item_p().recover_with(bad_item_recovery).repeated().collect::<Vec<_>>();
  let mut state = SimpleState(yag_state);

  let (opt_out, errors): (Option<Vec<AstItem>>, Vec<_>) = parser
    .parse_with_state(make_yag_parser_input(trees), &mut state)
    .into_output_errors();

  (opt_out.unwrap_or_default(), errors)
}

pub fn item_p<'src>() -> impl YagParser<'src, AstItem> {
  let mut attributes_p = Recursive::declare();
  let mut statement_p = Recursive::declare();
  let mut expr_p = Recursive::declare();
  let mut condition_p = Recursive::declare();
  let mut statement_body_p = Recursive::declare();
  let mut if_else_info_p = Recursive::declare();
  let mut loop_info_p = Recursive::declare();

  statement_body_p.define({
    statement_p
      .clone()
      .repeated()
      .collect::<Vec<_>>()
      .then(expr_p.clone().or_not())
      .nested_in(braces_content_p())
      .map(|(statements, tail_expr)| AstSatementBody { statements, tail_expr })
  });

  if_else_info_p.define({
    kw_if_p()
      .ignore_then(condition_p.clone())
      .then(statement_body_p.clone())
      .then(kw_else_p().ignore_then(statement_body_p.clone()).or_not())
      .map(|((condition, if_body), else_body)| IfElseInfo {
        condition,
        if_body,
        else_body,
      })
  });

  loop_info_p.define({
    let times_kw = StrID::from("times");
    let label = punct_quote_p()
      .ignore_then(spanned_ident_p())
      .then_ignore(punct_colon_p())
      .or_not();
    let times = expr_p
      .clone()
      .then_ignore(ident_p().filter(move |i| *i == times_kw))
      .or_not();

    label
      .then_ignore(kw_loop_p())
      .then(times)
      .then(statement_body_p.clone())
      .map(|((name, times), steps)| LoopInfo { name, steps, times })
  });

  attributes_p.define({
    let assign_kind = spanned_ident_p()
      .then_ignore(punct_equal_p())
      .then(expr_p.clone())
      .map(|((name, name_span), x)| AttributeKind::Assign(name, name_span, x));
    let expr_kind = expr_p.clone().map(|x| AttributeKind::Expr(x));
    //
    punct_hash_p()
      .ignore_then(
        choice((assign_kind, expr_kind))
          .recover_with(via_parser(
            any().repeated().to(AttributeKind::AttributeKindError),
          ))
          .nested_in(brackets_content_p())
          .map_with(|kind, ex| Attribute { span: ex.span(), kind }),
      )
      .labelled("attribute")
      .as_context()
      .repeated()
      .collect::<Vec<_>>()
  });

  statement_p.define({
    let let_kind = kw_let_p()
      .ignore_then(spanned_ident_p())
      .then(punct_colon_p().ignore_then(type_name_p()).or_not())
      .then(punct_equal_p().ignore_then(expr_p.clone()).or_not())
      .then_ignore(punct_semicolon_p().repeated().at_least(1))
      .map(|(((name, name_span), opt_ty), opt_init)| {
        StatementKind::Let(name, name_span, opt_ty, opt_init)
      })
      .labelled("let_statement")
      .as_context();
    let assign_kind = expr_p
      .clone()
      .then_ignore(punct_equal_p())
      .then(expr_p.clone())
      .then_ignore(punct_semicolon_p().repeated().at_least(1))
      .map(|(lhs, rhs)| StatementKind::Assign(lhs, rhs))
      .labelled("assignment_statement")
      .as_context();
    let bin_op_kind = expr_p
      .clone()
      .then(bin_op_assign_p())
      .then(expr_p.clone())
      .then_ignore(punct_semicolon_p().repeated().at_least(1))
      .map(|((lhs, bin), rhs)| StatementKind::BinOpAssign(lhs, bin, rhs))
      .labelled("bin_op_assign_statement")
      .as_context();
    let if_else_kind = if_else_info_p
      .clone()
      .map(StatementKind::IfElse)
      .then_ignore(punct_semicolon_p().repeated())
      .labelled("if_statement")
      .as_context();
    let loop_kind = loop_info_p
      .clone()
      .map(StatementKind::Loop)
      .then_ignore(punct_semicolon_p().repeated())
      .labelled("loop_statement")
      .as_context();
    let statement_recovery = via_parser(
      none_of([Lone(Token::Semicolon)])
        .repeated()
        .then(punct_semicolon_p())
        .to(StatementKind::StatementKindError),
    );
    attributes_p
      .clone()
      .then(
        choice((let_kind, assign_kind, bin_op_kind, if_else_kind, loop_kind))
          .recover_with(statement_recovery),
      )
      .map_with(|(attributes, kind), ex| Statement {
        attributes: if attributes.is_empty() {
          None
        } else {
          Some(Box::new(attributes))
        },
        kind: Box::new(kind),
        span: ex.span(),
      })
      .labelled("statement")
      .as_context()
  });

  expr_p.define(define_expr_p(
    expr_p.clone(),
    if_else_info_p.clone(),
    loop_info_p.clone(),
    statement_body_p.clone(),
    true,
  ));

  condition_p.define(define_expr_p(
    expr_p.clone(),
    if_else_info_p.clone(),
    loop_info_p.clone(),
    statement_body_p.clone(),
    false,
  ));

  let ast_function_p = {
    let fn_arg_p = spanned_ident_p()
      .then_ignore(punct_colon_p())
      .then(type_name_p())
      .map(|((name, name_span), ty)| {
        AstFunctionArgKind::NameTy(name, name_span, ty)
      })
      .recover_with(via_parser(
        none_of([Lone(Token::Comma)])
          .repeated()
          .at_least(1)
          .to(AstFunctionArgKind::AstFunctionArgKindError),
      ))
      .labelled("function_argument")
      .as_context();
    let fn_args_p = fn_arg_p
      .separated_by(punct_comma_p())
      .allow_trailing()
      .collect::<Vec<_>>()
      .nested_in(parens_content_p())
      .labelled("function_argument_group")
      .as_context();
    let return_ty_p = punct_minus_p()
      .ignore_then(punct_greater_than_p())
      .ignore_then(type_name_p())
      .or_not()
      .labelled("return_type")
      .as_context();
    attributes_p
      .clone()
      .then_ignore(kw_fn_p())
      .then(spanned_ident_p())
      .then(fn_args_p)
      .then(return_ty_p)
      .then(statement_body_p.clone())
      .map_with(
        |((((attributes, (name, name_span)), args), return_ty), body), ex| {
          AstItem {
            file_id: ex.state().file_id,
            span: ex.span(),
            attributes,
            kind: AstItemKind::Function(AstFunction {
              name,
              name_span,
              args,
              return_ty,
              body,
            }),
          }
        },
      )
      .labelled("function")
      .as_context()
  };
  let ast_bitbag_p = {
    let one_field = spanned_ident_p()
      .then_ignore(punct_colon_p())
      .then(expr_p.clone())
      .map(|((name, name_span), x)| Some((name, name_span, x)))
      .labelled("bitbag_field_definition")
      .as_context();
    let fields = one_field
      .separated_by(punct_comma_p())
      .allow_trailing()
      .collect::<Vec<_>>()
      .nested_in(braces_content_p());
    attributes_p
      .clone()
      .then_ignore(kw_bitbag_p())
      .then(spanned_ident_p())
      .then(fields)
      .map_with(|((attributes, (name, name_span)), fields), ex| AstItem {
        file_id: ex.state().file_id,
        span: ex.span(),
        attributes,
        kind: AstItemKind::Bitbag(AstBitbag { name, name_span, fields }),
      })
  };

  choice((ast_function_p, ast_bitbag_p)).labelled("item").as_context()
}

fn define_expr_p<'b, 'src: 'b>(
  expr_p: YagRecursive<'b, 'src, Expr>,
  if_else_info_p: YagRecursive<'b, 'src, IfElseInfo>,
  loop_info_p: YagRecursive<'b, 'src, LoopInfo>,
  statement_body_p: YagRecursive<'b, 'src, AstSatementBody>,
  include_struct_lit: bool,
) -> impl Parser<'src, YagParserInput<'src>, Expr, YagParserExtra<'src>> + Clone + 'b
{
  let atom = {
    let num_lit_kind = num_lit_p().map(ExprKind::NumLit);
    let str_lit_kind = str_lit_p().map(ExprKind::StrLit);
    let ident_kind = ident_p().map(ExprKind::Ident);
    let bool_kind = bool_p().map(ExprKind::Bool);
    let struct_lit_kind = {
      let struct_field_init_kind_p = {
        let set_init_kind = spanned_ident_p()
          .map(|(name, name_span)| StructFieldInitKind::Set(name, name_span));
        let assign_kind = spanned_ident_p()
          .then_ignore(punct_equal_p())
          .then(expr_p.clone())
          .map(|((name, name_span), x)| {
            StructFieldInitKind::Assign(name, name_span, x)
          });
        choice((assign_kind, set_init_kind))
          .labelled("struct_field_initializer")
          .as_context()
      };
      let fields = struct_field_init_kind_p
        .separated_by(punct_comma_p())
        .allow_trailing()
        .collect::<Vec<_>>()
        .nested_in(braces_content_p());
      spanned_ident_p()
        .then(fields)
        .map(|((name, name_span), inits)| {
          ExprKind::StructLit(name, name_span, inits)
        })
        .boxed()
    };
    let comma_separated_exprs = expr_p
      .clone()
      .separated_by(punct_comma_p())
      .allow_trailing()
      .collect::<Vec<_>>();
    let macro_kind = ident_p()
      .then_ignore(punct_exclamation_p())
      .then(comma_separated_exprs.clone().nested_in(parens_content_p()))
      .map(|(i, expressions)| ExprKind::Macro(i, expressions));
    let list_kind = comma_separated_exprs
      .clone()
      .nested_in(brackets_content_p())
      .map(ExprKind::List);
    let block_kind = statement_body_p.map(ExprKind::Block);
    let if_else_kind = if_else_info_p.map(ExprKind::IfElse);
    let loop_kind = loop_info_p.map(ExprKind::Loop);
    let continue_kind = kw_continue_p()
      .ignore_then(punct_quote_p().ignore_then(spanned_ident_p()).or_not())
      .map(ExprKind::Continue);

    let ident_choice = if include_struct_lit {
      choice((struct_lit_kind, macro_kind, ident_kind)).boxed()
    } else {
      choice((macro_kind, ident_kind)).boxed()
    };
    choice((
      ident_choice,
      num_lit_kind,
      str_lit_kind,
      bool_kind,
      list_kind,
      block_kind,
      if_else_kind,
      loop_kind,
      continue_kind,
    ))
    .map_with(|kind, ex| Expr { span: ex.span(), kind: Box::new(kind) })
    .or(expr_p.clone().nested_in(parens_content_p()))
  };

  let call_op = expr_p
    .clone()
    .separated_by(punct_comma_p())
    .allow_trailing()
    .collect::<Vec<_>>()
    .nested_in(parens_content_p())
    .map_with(|args, ex| Expr {
      span: ex.span(),
      kind: Box::new(ExprKind::List(args)),
    });
  let index_op = expr_p.clone().nested_in(brackets_content_p());

  use chumsky::pratt::*;
  let with_pratt = atom.pratt((
    // 3: range operators
    infix(left(4), short_circuit_or_p(), infix_maker!(BinOpKind::BoolOr)),
    infix(left(5), short_circuit_and_p(), infix_maker!(BinOpKind::BoolAnd)),
    infix(left(6), cmp_eq_p(), infix_maker!(BinOpKind::Eq)),
    infix(left(6), cmp_ne_p(), infix_maker!(BinOpKind::Ne)),
    infix(left(6), cmp_lt_p(), infix_maker!(BinOpKind::Lt)),
    infix(left(6), cmp_gt_p(), infix_maker!(BinOpKind::Gt)),
    infix(left(6), cmp_le_p(), infix_maker!(BinOpKind::Le)),
    infix(left(6), cmp_ge_p(), infix_maker!(BinOpKind::Ge)),
    infix(left(7), punct_pipe_p(), infix_maker!(BinOpKind::BitOr)),
    infix(left(8), punct_caret_p(), infix_maker!(BinOpKind::BitXor)),
    infix(left(9), punct_ampersand_p(), infix_maker!(BinOpKind::BitAnd)),
    infix(left(10), shl_p(), infix_maker!(BinOpKind::ShiftLeft)),
    infix(left(10), shr_p(), infix_maker!(BinOpKind::ShiftRight)),
    infix(left(11), punct_plus_p(), infix_maker!(BinOpKind::Add)),
    infix(left(11), punct_minus_p(), infix_maker!(BinOpKind::Sub)),
    infix(left(12), punct_asterisk_p(), infix_maker!(BinOpKind::Mul)),
    infix(left(12), punct_slash_p(), infix_maker!(BinOpKind::Div)),
    infix(left(12), punct_percent_p(), infix_maker!(BinOpKind::Mod)),
    prefix(13, punct_minus_p(), prefix_maker!(UnOpKind::Neg)),
    prefix(13, punct_exclamation_p(), prefix_maker!(UnOpKind::Not)),
    prefix(13, punct_asterisk_p(), prefix_maker!(UnOpKind::Deref)),
    prefix(13, punct_ampersand_p(), prefix_maker!(UnOpKind::Ref)),
    prefix(13, punct_backtick_p(), prefix_maker!(UnOpKind::Backtick)),
    // 14: question-mark-operator
    postfix(15, call_op, |lhs, rhs, extras| Expr {
      span: extras.span(),
      kind: Box::new(ExprKind::BinOp(lhs, BinOpKind::Call, rhs)),
    }),
    postfix(15, index_op, |lhs, rhs, extras| Expr {
      span: extras.span(),
      kind: Box::new(ExprKind::BinOp(lhs, BinOpKind::Index, rhs)),
    }),
    infix(left(16), punct_period_p(), infix_maker!(BinOpKind::Dot)),
    // 17: method calls
    // 18: path
  ));

  with_pratt
}

#[derive(Debug, Clone, Copy)]
pub struct YagParserState {
  pub source: &'static str,
  pub file_id: FileID,
}

pub type YagParserInput<'src> = MappedInput<
  'src,
  TokenTree,
  Span32,
  &'src [(TokenTree, Span32)],
  fn(&'src (TokenTree, Span32)) -> (&'src TokenTree, &'src Span32),
>;

pub type YagParserExtra<'src> =
  Full<Rich<'src, TokenTree, Span32>, SimpleState<YagParserState>, ()>;

pub type YagRecursive<'b, 'src, T> =
  Recursive<Indirect<'src, 'b, YagParserInput<'src>, T, YagParserExtra<'src>>>;

fn mapper<'src>(
  (tt, span): &'src (TokenTree, Span32),
) -> (&'src TokenTree, &'src Span32) {
  (tt, span)
}

pub fn make_yag_parser_input<'src>(
  s: &'src [(TokenTree, Span32)],
) -> YagParserInput<'src> {
  let eoi: Span32 = s.last().map(|(_tt, span)| *span).unwrap_or_default();
  Input::map(s, eoi, mapper)
}

pub trait YagParser<'src, O>:
  Parser<'src, YagParserInput<'src>, O, YagParserExtra<'src>> + Clone
{
}

impl<'src, O, T> YagParser<'src, O> for T
where
  T: Parser<'src, YagParserInput<'src>, O, YagParserExtra<'src>>,
  T: Clone,
{
}

/// Lets us assert a particular expected output type for the parser.
///
/// This is necesary because rust doesn't allow a let binding to use "impl
/// trait" as the type declaration.
///
/// No runtime effect.
pub fn assert_output_ty<'src, T>(_: &impl YagParser<'src, T>) {}

#[test]
fn test_impl_return_readabilty() {
  #[allow(dead_code)]
  fn example_parser<'src>() -> impl YagParser<'src, ()> {
    chumsky::prelude::todo()
  }
}

pub fn braces_content_p<'src>() -> impl YagParser<'src, YagParserInput<'src>> {
  select_ref! {
    TokenTree::Braces(b) => make_yag_parser_input(b),
  }
  .labelled("Braces")
  .as_context()
}
pub fn brackets_content_p<'src>() -> impl YagParser<'src, YagParserInput<'src>>
{
  select_ref! {
    TokenTree::Brackets(b) => make_yag_parser_input(b),
  }
  .labelled("Brackets")
  .as_context()
}
pub fn parens_content_p<'src>() -> impl YagParser<'src, YagParserInput<'src>> {
  select_ref! {
    TokenTree::Parens(b) => make_yag_parser_input(b),
  }
  .labelled("Parens")
  .as_context()
}

pub fn kw_bitbag_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwBitbag) => ()
  }
  .labelled("`bitbag`")
  .as_context()
}
pub fn kw_break_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwBreak) => ()
  }
  .labelled("`break`")
  .as_context()
}
pub fn kw_const_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwConst) => ()
  }
  .labelled("`const`")
  .as_context()
}
pub fn kw_continue_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwContinue) => ()
  }
  .labelled("`continue`")
  .as_context()
}
pub fn kw_else_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwElse) => ()
  }
  .labelled("`else`")
  .as_context()
}
pub fn kw_fn_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwFn) => ()
  }
  .labelled("`fn`")
  .as_context()
}
pub fn kw_if_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwIf) => ()
  }
  .labelled("`if`")
  .as_context()
}
pub fn kw_let_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwLet) => ()
  }
  .labelled("`let`")
  .as_context()
}
pub fn kw_loop_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwLoop) => ()
  }
  .labelled("`loop`")
  .as_context()
}
pub fn kw_mmio_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwMmio) => ()
  }
  .labelled("`mmio`")
  .as_context()
}
pub fn kw_ram_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwRam) => ()
  }
  .labelled("`ram`")
  .as_context()
}
pub fn kw_return_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwReturn) => ()
  }
  .labelled("`return`")
  .as_context()
}
pub fn kw_rom_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwRom) => (),
  }
  .labelled("`rom`")
  .as_context()
}
pub fn kw_static_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwStatic) => ()
  }
  .labelled("`static`")
  .as_context()
}
pub fn kw_struct_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwStruct) => ()
  }
  .labelled("`struct`")
  .as_context()
}

pub fn punct_asterisk_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(Asterisk) => ()
  }
  .labelled("`*`")
  .as_context()
}
pub fn punct_ampersand_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(Ampersand) => ()
  }
  .labelled("`&`")
  .as_context()
}
pub fn punct_backtick_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(Backtick) => ()
  }
  .labelled("backtick")
  .as_context()
}
pub fn punct_caret_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(Caret) => ()
  }
  .labelled("`^`")
  .as_context()
}
pub fn punct_comma_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(Comma) => ()
  }
  .labelled("`,`")
  .as_context()
}
pub fn punct_colon_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(Colon) => ()
  }
  .labelled("`:`")
  .as_context()
}
pub fn punct_greater_than_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(GreaterThan) => ()
  }
  .labelled("`>`")
  .as_context()
}
pub fn punct_equal_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(Equal) => ()
  }
  .labelled("`=`")
  .as_context()
}
pub fn punct_exclamation_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(Exclamation) => ()
  }
  .labelled("`!`")
  .as_context()
}
pub fn punct_hash_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(Hash) => ()
  }
  .labelled("`#`")
  .as_context()
}
pub fn punct_less_than_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(LessThan) => ()
  }
  .labelled("`<`")
  .as_context()
}
pub fn punct_minus_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(Minus) => ()
  }
  .labelled("`-`")
  .as_context()
}
pub fn punct_percent_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(Percent) => ()
  }
  .labelled("`%`")
  .as_context()
}
pub fn punct_period_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(Period) => ()
  }
  .labelled("`.`")
  .as_context()
}
pub fn punct_pipe_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(Pipe) => ()
  }
  .labelled("`|`")
  .as_context()
}
pub fn punct_plus_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(Plus) => ()
  }
  .labelled("`+`")
  .as_context()
}
pub fn punct_quote_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(Quote) => ()
  }
  .labelled("`'`")
  .as_context()
}
pub fn punct_semicolon_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(Semicolon) => ()
  }
  .labelled("`;`")
  .as_context()
}
pub fn punct_slash_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(Slash) => ()
  }
  .labelled("`/`")
  .as_context()
}

/// `==`
pub fn cmp_eq_p<'src>() -> impl YagParser<'src, ()> {
  punct_equal_p().ignore_then(punct_equal_p()).labelled("`==`").as_context()
}
/// `!=`
pub fn cmp_ne_p<'src>() -> impl YagParser<'src, ()> {
  punct_exclamation_p()
    .ignore_then(punct_equal_p())
    .labelled("`!=`")
    .as_context()
}
/// `>`
pub fn cmp_gt_p<'src>() -> impl YagParser<'src, ()> {
  punct_greater_than_p()
}
/// `<`
pub fn cmp_lt_p<'src>() -> impl YagParser<'src, ()> {
  punct_less_than_p()
}
/// `>=`
pub fn cmp_ge_p<'src>() -> impl YagParser<'src, ()> {
  punct_greater_than_p()
    .ignore_then(punct_equal_p())
    .labelled("`>=`")
    .as_context()
}
/// `<=`
pub fn cmp_le_p<'src>() -> impl YagParser<'src, ()> {
  punct_less_than_p().ignore_then(punct_equal_p()).labelled("`<=`").as_context()
}
/// `&&`
pub fn short_circuit_and_p<'src>() -> impl YagParser<'src, ()> {
  punct_ampersand_p()
    .ignore_then(punct_ampersand_p())
    .labelled("`&&`")
    .as_context()
}
/// `||`
pub fn short_circuit_or_p<'src>() -> impl YagParser<'src, ()> {
  punct_pipe_p().ignore_then(punct_pipe_p()).labelled("`||`").as_context()
}
/// `>>`
pub fn shr_p<'src>() -> impl YagParser<'src, ()> {
  punct_greater_than_p()
    .ignore_then(punct_greater_than_p())
    .labelled("`>>`")
    .as_context()
}
/// `<<`
pub fn shl_p<'src>() -> impl YagParser<'src, ()> {
  punct_less_than_p()
    .ignore_then(punct_less_than_p())
    .labelled("`<<`")
    .as_context()
}

pub fn ident_p<'src>() -> impl YagParser<'src, StrID> {
  select! {
    Lone(Ident) = ex => {
      let state: &SimpleState<YagParserState> = ex.state();
      let source: &str = state.source;
      let span: Span32 = ex.span();
      let range: Range<usize> = span.start.try_into().unwrap()..span.end.try_into().unwrap();
      let str_id = StrID::from(&source[range]);
      str_id
    }
  }
  .labelled("identifier")
  .as_context()
}
pub fn spanned_ident_p<'src>() -> impl YagParser<'src, (StrID, Span32)> {
  ident_p().map_with(|i, ex| (i, ex.span()))
}
pub fn num_lit_p<'src>() -> impl YagParser<'src, StrID> {
  select! {
    Lone(NumLit) = ex => {
      let state: &SimpleState<YagParserState> = ex.state();
      let source: &str = state.source;
      let span: Span32 = ex.span();
      let range: Range<usize> = span.start.try_into().unwrap()..span.end.try_into().unwrap();
      let str_id = StrID::from(&source[range]);
      str_id
    }
  }
  .labelled("number")
  .as_context()
}
pub fn str_lit_p<'src>() -> impl YagParser<'src, StrID> {
  select! {
    Lone(StrLit) = ex => {
      let state: &SimpleState<YagParserState> = ex.state();
      let source: &str = state.source;
      let span: Span32 = ex.span();
      let range: Range<usize> = span.start.try_into().unwrap()..span.end.try_into().unwrap();
      let str_id = StrID::from(&source[range]);
      str_id
    }
  }
  .labelled("str")
  .as_context()
}
pub fn bool_p<'src>() -> impl YagParser<'src, bool> {
  select! {
    Lone(KwTrue) => true,
    Lone(KwFalse) => false,
  }
  .labelled("boolean")
  .as_context()
}

pub fn type_name_p<'src>() -> impl YagParser<'src, TypeName> {
  recursive(|type_name_kind| {
    let ident_kind = ident_p().map(TypeNameKind::Ident);
    let array_num_lit_kind = type_name_kind
      .clone()
      .then_ignore(punct_semicolon_p())
      .then(num_lit_p())
      .nested_in(brackets_content_p())
      .map(|(kind, num)| TypeNameKind::ArrayNumLit(Box::new(kind), num));
    let array_ident_kind = type_name_kind
      .clone()
      .then_ignore(punct_semicolon_p())
      .then(ident_p())
      .nested_in(brackets_content_p())
      .map(|(kind, ident)| TypeNameKind::ArrayIdent(Box::new(kind), ident));
    choice((ident_kind, array_num_lit_kind, array_ident_kind)).recover_with(
      via_parser(
        any()
          .repeated()
          .nested_in(brackets_content_p())
          .to(TypeNameKind::TypeNameKindError),
      ),
    )
  })
  .map_with(|kind, ex| TypeName { span: ex.span(), kind })
  .labelled("type_name")
  .as_context()
}

pub fn bin_op_assign_p<'src>() -> impl YagParser<'src, BinOpKind> {
  choice((
    punct_plus_p().ignore_then(punct_equal_p()).to(BinOpKind::Add),
    punct_minus_p().ignore_then(punct_equal_p()).to(BinOpKind::Sub),
    punct_asterisk_p().ignore_then(punct_equal_p()).to(BinOpKind::Mul),
    punct_slash_p().ignore_then(punct_equal_p()).to(BinOpKind::Div),
    punct_percent_p().ignore_then(punct_equal_p()).to(BinOpKind::Mod),
    shl_p().ignore_then(punct_equal_p()).to(BinOpKind::ShiftLeft),
    shr_p().ignore_then(punct_equal_p()).to(BinOpKind::ShiftRight),
    punct_ampersand_p().ignore_then(punct_equal_p()).to(BinOpKind::BitAnd),
    punct_pipe_p().ignore_then(punct_equal_p()).to(BinOpKind::BitOr),
    punct_caret_p().ignore_then(punct_equal_p()).to(BinOpKind::BitXor),
  ))
  .labelled("bin_op_assign")
  .as_context()
}
