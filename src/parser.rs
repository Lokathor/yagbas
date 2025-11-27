use super::*;
use chumsky::prelude::*;
use chumsky::recovery::Strategy;
use chumsky::{
  Parser,
  error::Rich,
  extra::{Full, SimpleState},
  input::{BorrowInput, Input, MappedInput, ValueInput},
};

#[derive(Debug, Clone, Copy)]
pub struct YagParserState {
  pub source: &'static str,
}

pub type YagParserInput<'src> = MappedInput<
  TokenTree,
  Span32,
  &'src [(TokenTree, Span32)],
  fn(&'src (TokenTree, Span32)) -> (&'src TokenTree, &'src Span32),
>;

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
  Parser<
    'src,
    YagParserInput<'src>,
    O,
    Full<Rich<'src, TokenTree, Span32>, SimpleState<YagParserState>, ()>,
  > + Clone
{
}

impl<'src, O, T> YagParser<'src, O> for T
where
  T: Parser<
      'src,
      YagParserInput<'src>,
      O,
      Full<Rich<'src, TokenTree, Span32>, SimpleState<YagParserState>, ()>,
    >,
  T: Clone,
{
}

/// Lets us assert a particular expected output type for the parser.
///
/// This is necesary because rust doesn't allow a let binding to use "impl
/// trait" as the type declaration.
///
/// No runtime effect.
fn assert_output_ty<'src, T>(_: &impl YagParser<'src, T>) {}

#[test]
fn test_impl_return_readabilty() {
  #[allow(dead_code)]
  fn example_parser<'src>() -> impl YagParser<'src, ()> {
    chumsky::prelude::todo()
  }
}

use crate::Token::*;
use crate::TokenTree::*;

/// Lets you `select_ref!` the content out of some `Braces`
pub fn braces_content_p<'src>() -> impl YagParser<'src, YagParserInput<'src>> {
  select_ref! {
    TokenTree::Braces(b) = ex => make_yag_parser_input(b),
  }
}
/// Lets you `select_ref!` the content out of some `Brackets`
pub fn brackets_content_p<'src>() -> impl YagParser<'src, YagParserInput<'src>>
{
  select_ref! {
    TokenTree::Brackets(b) = ex => make_yag_parser_input(b),
  }
}
/// Lets you `select_ref!` the content out of some `Parens`
pub fn parens_content_p<'src>() -> impl YagParser<'src, YagParserInput<'src>> {
  select_ref! {
    TokenTree::Parens(b) = ex => make_yag_parser_input(b),
  }
}

pub fn kw_bitbag_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwBitbag) => ()
  }
}
pub fn kw_break_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwBreak) => ()
  }
}
pub fn kw_const_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwConst) => ()
  }
}
pub fn kw_continue_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwContinue) => ()
  }
}
pub fn kw_else_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwElse) => ()
  }
}
pub fn kw_fn_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwFn) => ()
  }
}
pub fn kw_if_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwIf) => ()
  }
}
pub fn kw_let_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwLet) => ()
  }
}
pub fn kw_loop_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwLoop) => ()
  }
}
pub fn kw_mmio_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwMmio) => ()
  }
}
pub fn kw_mut_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwMut) => ()
  }
}
pub fn kw_return_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwReturn) => ()
  }
}
pub fn kw_rom_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwRom) => ()
  }
}
pub fn kw_static_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwStatic) => ()
  }
}
pub fn kw_struct_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwStruct) => ()
  }
}

pub fn punct_asterisk_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(Asterisk) => ()
  }
}
pub fn punct_ampersand_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(Ampersand) => ()
  }
}
pub fn punct_caret_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(Caret) => ()
  }
}
pub fn punct_comma_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(Comma) => ()
  }
}
pub fn punct_colon_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(Colon) => ()
  }
}
pub fn punct_greater_than_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(GreaterThan) => ()
  }
}
pub fn punct_equal_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(Equal) => ()
  }
}
pub fn punct_exclamation_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(Exclamation) => ()
  }
}
pub fn punct_hash_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(Hash) => ()
  }
}
pub fn punct_less_than_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(LessThan) => ()
  }
}
pub fn punct_minus_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(Minus) => ()
  }
}
pub fn punct_percent_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(Percent) => ()
  }
}
pub fn punct_period_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(Period) => ()
  }
}
pub fn punct_pipe_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(Pipe) => ()
  }
}
pub fn punct_plus_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(Plus) => ()
  }
}
pub fn punct_quote_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(Quote) => ()
  }
}
pub fn punct_semicolon_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(Semicolon) => ()
  }
}
pub fn punct_slash_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(Slash) => ()
  }
}

/// `==`
pub fn cmp_eq_p<'src>() -> impl YagParser<'src, ()> {
  punct_equal_p().ignore_then(punct_equal_p())
}
/// `!=`
pub fn cmp_ne_p<'src>() -> impl YagParser<'src, ()> {
  punct_exclamation_p().ignore_then(punct_equal_p())
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
  punct_greater_than_p().ignore_then(punct_equal_p())
}
/// `<=`
pub fn cmp_le_p<'src>() -> impl YagParser<'src, ()> {
  punct_less_than_p().ignore_then(punct_equal_p())
}
/// `&&`
pub fn short_circuit_and_p<'src>() -> impl YagParser<'src, ()> {
  punct_ampersand_p().ignore_then(punct_ampersand_p())
}
/// `||`
pub fn short_circuit_or_p<'src>() -> impl YagParser<'src, ()> {
  punct_pipe_p().ignore_then(punct_pipe_p())
}
/// `>>`
pub fn shr_p<'src>() -> impl YagParser<'src, ()> {
  punct_greater_than_p().ignore_then(punct_greater_than_p())
}
/// `<<`
pub fn shl_p<'src>() -> impl YagParser<'src, ()> {
  punct_less_than_p().ignore_then(punct_less_than_p())
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
}

pub fn bool_p<'src>() -> impl YagParser<'src, bool> {
  select! {
    Lone(KwTrue) => true,
    Lone(KwFalse) => false,
  }
}

pub fn expr_p<'src>() -> impl YagParser<'src, Expr> {
  expr_p_and_statement_p().0
}
pub fn statement_p<'src>() -> impl YagParser<'src, Statement> {
  expr_p_and_statement_p().1
}

macro_rules! infix_maker {
  ($kind: path) => {
    |lhs, _op, rhs, extras| Expr {
      span: extras.span(),
      kind: Box::new(ExprKind::BinOp(ExprBinOp { lhs, rhs, kind: $kind })),
    }
  };
}
macro_rules! prefix_maker {
  ($kind: path) => {
    |_op, inner, extras| Expr {
      span: extras.span(),
      kind: Box::new(ExprKind::UnOp(ExprUnOp { inner, kind: $kind })),
    }
  };
}
#[allow(unused)]
macro_rules! postfix_maker {
  ($kind: path) => {
    |atom, _op, extras| todo!()
  };
}
macro_rules! define_expression_parser {
  ($expression_parser:expr, $statement_parser:expr) => {{
    let atom = {
      let num_lit = num_lit_p().map(|lit| ExprKind::NumLit(ExprNumLit { lit }));
      let ident = ident_p().map(|ident| ExprKind::Ident(ExprIdent { ident }));
      let bool_ = bool_p().map(|b| ExprKind::Bool(b));
      let list = $expression_parser
        .clone()
        .separated_by(punct_comma_p())
        .allow_trailing()
        .collect::<Vec<_>>()
        .nested_in(brackets_content_p())
        .map(|elements| ExprKind::List(ExprList { elements }));
      // TODO: we need to somehow track when a body has a trailing semicolon.
      let block = $statement_parser
        .clone()
        .separated_by(punct_semicolon_p())
        .allow_trailing()
        .collect::<Vec<_>>()
        .nested_in(braces_content_p())
        .map(|body| ExprKind::Block(ExprBlock { body }));
      let call = ident_p()
        .map_with(|i, ex| (i, ex.span()))
        .then(
          $expression_parser
            .clone()
            .separated_by(punct_comma_p())
            .allow_trailing()
            .collect::<Vec<_>>()
            .nested_in(parens_content_p()),
        )
        .map(|((target, target_span), args)| {
          ExprKind::Call(ExprCall { target, target_span, args })
        });
      let macro_ = ident_p()
        .map_with(|i, ex| (i, ex.span()))
        .then_ignore(punct_exclamation_p())
        .then(
          $expression_parser
            .clone()
            .separated_by(punct_comma_p())
            .allow_trailing()
            .collect::<Vec<_>>()
            .nested_in(parens_content_p()),
        )
        .map(|((target, target_span), args)| {
          ExprKind::Macro(ExprMacro { target, target_span, args })
        });
      let struct_lit = ident_p()
        .map_with(|i, ex| (i, ex.span()))
        .then(
          $expression_parser
            .clone()
            .separated_by(punct_comma_p())
            .allow_trailing()
            .collect::<Vec<_>>()
            .nested_in(braces_content_p()),
        )
        .map(|((ty, ty_span), args)| {
          ExprKind::StructLit(ExprStructLit { ty, ty_span, args })
        });
      let if_else = kw_if_p()
        .ignore_then($expression_parser.clone())
        .then(
          $statement_parser
            .clone()
            .separated_by(punct_semicolon_p())
            .allow_trailing()
            .collect::<Vec<_>>()
            .nested_in(braces_content_p()),
        )
        .then(
          kw_else_p()
            .ignore_then(
              $statement_parser
                .clone()
                .separated_by(punct_semicolon_p())
                .allow_trailing()
                .collect::<Vec<_>>()
                .nested_in(braces_content_p()),
            )
            .or_not(),
        )
        .map(|((condition, if_body), else_body)| {
          ExprKind::IfElse(ExprIfElse {
            condition,
            if_body,
            else_body: else_body.unwrap_or_default(),
          })
        });
      let loop_ = punct_quote_p()
        .ignore_then(ident_p())
        .then_ignore(punct_colon_p())
        .or_not()
        .then_ignore(kw_loop_p())
        .then(
          $statement_parser
            .clone()
            .separated_by(punct_semicolon_p())
            .allow_trailing()
            .collect::<Vec<_>>()
            .nested_in(braces_content_p()),
        )
        .map(|(name, body)| ExprKind::Loop(ExprLoop { name, body }));
      // TODO: loop times
      let break_ = kw_break_p()
        .ignore_then(
          punct_quote_p()
            .ignore_then(ident_p().map_with(|i, ex| (i, ex.span())))
            .or_not(),
        )
        .then($expression_parser.clone().or_not())
        .map(|(target, value)| ExprKind::Break(ExprBreak { target, value }));
      let continue_ = kw_continue_p()
        .ignore_then(
          punct_quote_p()
            .ignore_then(ident_p().map_with(|i, ex| (i, ex.span())))
            .or_not(),
        )
        .map(|target| ExprKind::Continue(ExprContinue { target }));
      let return_ = kw_return_p()
        .ignore_then($expression_parser.clone().or_not())
        .map(|value| ExprKind::Return(ExprReturn { value }));
      // TODO: can we prevent the boxing of the inner expression which we then
      // immediately unbox?
      let paren_group_expr = $expression_parser
        .clone()
        .nested_in(parens_content_p())
        .map(|xpr| *xpr.kind);

      let ident_using = choice((call, macro_, struct_lit, ident));
      let loop_using = choice((loop_,));
      choice((
        num_lit,
        ident_using,
        bool_,
        list,
        block,
        if_else,
        loop_using,
        break_,
        continue_,
        return_,
        paren_group_expr,
      ))
      .map_with(|kind, ex| Expr { span: ex.span(), kind: Box::new(kind) })
    };
    assert_output_ty::<Expr>(&atom);

    use chumsky::pratt::*;
    let with_pratt = atom.pratt((
      infix(left(2), punct_equal_p(), infix_maker!(BinOpKind::Assign)),
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
      // 14: question-mark-operator
      // 15: fn-call and index
      infix(left(16), punct_period_p(), infix_maker!(BinOpKind::Dot)),
      // 17: method calls
      // 18: path
    ));

    with_pratt
  }};
}
macro_rules! define_statement_parser {
  ($expression_parser:expr, $statement_parser:expr) => {{
    let attributes = punct_hash_p()
      .ignore_then($expression_parser.clone().nested_in(brackets_content_p()))
      .repeated()
      .collect::<Vec<_>>();
    let kind = {
      let let_ = kw_let_p()
        .ignore_then(ident_p())
        .then(punct_colon_p().ignore_then(ident_p()).or_not())
        .then(punct_equal_p().ignore_then($expression_parser.clone()).or_not())
        .map(|((varname, opt_ty), opt_init)| match opt_init {
          Some(init) => StatementKind::LetAssign(varname, opt_ty, init),
          None => StatementKind::Let(varname, opt_ty),
        });
      let xpr = $expression_parser.clone().map(|x| StatementKind::Expr(x));

      choice((let_, xpr))
    };
    attributes.then(kind).map_with(|(the_attributes, kind), ex| Statement {
      span: ex.span(),
      attribues: if the_attributes.is_empty() {
        None
      } else {
        Some(Box::new(the_attributes))
      },
      kind: Box::new(kind),
    })
  }};
}
pub fn expr_p_and_statement_p<'src>()
-> (impl YagParser<'src, Expr>, impl YagParser<'src, Statement>) {
  let mut expression_parser = Recursive::declare();
  let mut statement_parser = Recursive::declare();
  expression_parser
    .define(define_expression_parser!(expression_parser, statement_parser));
  statement_parser
    .define(define_statement_parser!(expression_parser, statement_parser));

  (expression_parser, statement_parser)
}
