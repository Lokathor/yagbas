use super::*;
use chumsky::prelude::*;
use chumsky::recovery::Strategy;
use chumsky::recursive::Indirect;
use chumsky::{
  Parser,
  error::Rich,
  extra::{Full, SimpleState},
  input::{BorrowInput, Input, MappedInput, ValueInput},
};

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

type YagRecursive<'b, 'src, T> =
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
    TokenTree::Braces(b) => make_yag_parser_input(b),
  }
  .labelled("braces")
  .as_context()
}
/// Lets you `select_ref!` the content out of some `Brackets`
pub fn brackets_content_p<'src>() -> impl YagParser<'src, YagParserInput<'src>>
{
  select_ref! {
    TokenTree::Brackets(b) => make_yag_parser_input(b),
  }
  .labelled("brackets")
  .as_context()
}
/// Lets you `select_ref!` the content out of some `Parens`
pub fn parens_content_p<'src>() -> impl YagParser<'src, YagParserInput<'src>> {
  select_ref! {
    TokenTree::Parens(b) => make_yag_parser_input(b),
  }
  .labelled("parens")
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

pub fn expr_p<'src>() -> impl YagParser<'src, Expr> {
  recursive_parser_group_p().0.labelled("expression").as_context()
}
pub fn statement_p<'src>() -> impl YagParser<'src, Statement> {
  recursive_parser_group_p().2.labelled("statement").as_context()
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

fn define_expression_parser<'b, 'src: 'b>(
  expression_parser: &mut YagRecursive<'b, 'src, Expr>,
  condition_parser: &mut YagRecursive<'b, 'src, Expr>,
  statement_parser: &mut YagRecursive<'b, 'src, Statement>,
  define_conditions: bool,
) {
  let mut target = if define_conditions {
    condition_parser.clone()
  } else {
    expression_parser.clone()
  };
  target.define({
    let atom = {
      let statement_body_p = statement_parser
        .clone()
        .then_ignore(punct_semicolon_p())
        .repeated()
        .collect::<Vec<_>>()
        .then(expression_parser.clone().or_not())
        .nested_in(braces_content_p())
        .map(|(body, last_expr)| StatementBody { body, last_expr })
        .labelled("statement_body")
        .as_context();
      let num_lit = num_lit_p().map(|lit| ExprKind::NumLit(lit));
      let str_lit = str_lit_p().map(|lit| ExprKind::StrLit(lit));
      let ident = ident_p().map(|ident| ExprKind::Ident(ident));
      let bool_ = bool_p().map(|b| ExprKind::Bool(b));
      let list = expression_parser
        .clone()
        .separated_by(punct_comma_p())
        .allow_trailing()
        .collect::<Vec<_>>()
        .nested_in(brackets_content_p())
        .map(ExprKind::List);
      let block = statement_body_p.clone().map(ExprKind::Block);
      let macro_ = spanned_ident_p()
        .then_ignore(punct_exclamation_p())
        .then(
          expression_parser
            .clone()
            .separated_by(punct_comma_p())
            .allow_trailing()
            .collect::<Vec<_>>()
            .nested_in(parens_content_p()),
        )
        .map(|((target, target_span), args)| {
          ExprKind::Macro(ExprMacro { target, target_span, args })
        });
      let struct_lit = spanned_ident_p()
        .then(
          choice((
            spanned_ident_p()
              .then_ignore(punct_equal_p())
              .then(expression_parser.clone())
              .map(|((f, f_span), xpr)| {
                StructLitFieldInitKind::Assign(f, f_span, xpr)
              }),
            spanned_ident_p()
              .map(|(f, f_span)| StructLitFieldInitKind::Activated(f, f_span)),
          ))
          .separated_by(punct_comma_p())
          .allow_trailing()
          .collect::<Vec<_>>()
          .nested_in(braces_content_p()),
        )
        .map(|((ty, ty_span), args)| {
          ExprKind::StructLit(ExprStructLit { ty, ty_span, args })
        });
      let if_else = kw_if_p()
        .ignore_then(condition_parser.clone())
        .then(statement_body_p.clone())
        .then(kw_else_p().ignore_then(statement_body_p.clone()).or_not())
        .map(|((condition, if_), else_)| {
          ExprKind::IfElse(ExprIfElse { condition, if_, else_ })
        });
      let loop_ = punct_quote_p()
        .ignore_then(spanned_ident_p())
        .then_ignore(punct_colon_p())
        .or_not()
        .then_ignore(kw_loop_p())
        .then(statement_body_p.clone())
        .map(|(name, steps)| ExprKind::Loop(ExprLoop { name, steps }));
      let times_kw = StrID::from("times");
      let loop_times = punct_quote_p()
        .ignore_then(spanned_ident_p())
        .then_ignore(punct_colon_p())
        .or_not()
        .then_ignore(kw_loop_p())
        .then(choice((ident_p(), num_lit_p())).map_with(|i, ex| (i, ex.span())))
        .then_ignore(ident_p().filter(move |str_id| *str_id == times_kw))
        .then(statement_body_p.clone())
        .map(|((name, (times, times_span)), steps)| {
          ExprKind::LoopTimes(ExprLoopTimes { name, times, times_span, steps })
        });
      let break_ = kw_break_p()
        .ignore_then(punct_quote_p().ignore_then(spanned_ident_p()).or_not())
        .then(expression_parser.clone().or_not())
        .map(|(target, value)| ExprKind::Break(ExprBreak { target, value }));
      let continue_ = kw_continue_p()
        .ignore_then(punct_quote_p().ignore_then(spanned_ident_p()).or_not())
        .map(|target| ExprKind::Continue(ExprContinue { target }));
      let return_ = kw_return_p()
        .ignore_then(expression_parser.clone().or_not())
        .map(|value| ExprKind::Return(ExprReturn { value }));
      // TODO: can we prevent the boxing of the inner expression which we then
      // immediately unbox?
      let paren_group_expr = expression_parser
        .clone()
        .nested_in(parens_content_p())
        .map(|xpr| *xpr.kind);

      let ident_using = if define_conditions {
        choice((macro_, ident)).boxed()
      } else {
        choice((macro_, struct_lit, ident)).boxed()
      };
      let loop_using = choice((loop_times, loop_));
      choice((
        num_lit,
        str_lit,
        ident_using,
        bool_,
        list,
        if_else,
        loop_using,
        break_,
        continue_,
        return_,
        block,
        paren_group_expr,
      ))
      .map_with(|kind, ex| Expr { span: ex.span(), kind: Box::new(kind) })
      .labelled("expression_atom")
      .as_context()
    };
    assert_output_ty::<Expr>(&atom);

    let call_op = expression_parser
      .clone()
      .separated_by(punct_comma_p())
      .allow_trailing()
      .collect::<Vec<_>>()
      .nested_in(parens_content_p())
      .map_with(|args, ex| Expr {
        span: ex.span(),
        kind: Box::new(ExprKind::List(args)),
      });
    assert_output_ty::<Expr>(&call_op);

    let index_op = expression_parser.clone().nested_in(brackets_content_p());
    assert_output_ty::<Expr>(&index_op);

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
        kind: Box::new(ExprKind::BinOp(ExprBinOp {
          lhs,
          rhs,
          kind: BinOpKind::Call,
        })),
      }),
      postfix(15, index_op, |lhs, rhs, extras| Expr {
        span: extras.span(),
        kind: Box::new(ExprKind::BinOp(ExprBinOp {
          lhs,
          rhs,
          kind: BinOpKind::Index,
        })),
      }),
      infix(left(16), punct_period_p(), infix_maker!(BinOpKind::Dot)),
      // 17: method calls
      // 18: path
    ));

    with_pratt
  });
}

fn define_statement_parser<'b, 'src: 'b>(
  expression_parser: &mut YagRecursive<'b, 'src, Expr>,
  _condition_parser: &mut YagRecursive<'b, 'src, Expr>,
  statement_parser: &mut YagRecursive<'b, 'src, Statement>,
) {
  statement_parser.define({
    let attributes = punct_hash_p()
      .ignore_then(expression_parser.clone().nested_in(brackets_content_p()))
      .repeated()
      .collect::<Vec<_>>();
    let kind = {
      let let_ = kw_let_p()
        .ignore_then(ident_p())
        .then(punct_colon_p().ignore_then(type_name_p()).or_not())
        .then(punct_equal_p().ignore_then(expression_parser.clone()).or_not())
        .then_ignore(punct_semicolon_p())
        .map(|((varname, opt_ty), opt_init)| match opt_init {
          Some(init) => StatementKind::LetAssign(varname, opt_ty, init),
          None => StatementKind::Let(varname, opt_ty),
        });
      let assign = expression_parser
        .clone()
        .then_ignore(punct_equal_p())
        .then(expression_parser.clone())
        .then_ignore(punct_semicolon_p())
        .map(|(l, r)| StatementKind::Assign(l, r));
      let add_assign = expression_parser
        .clone()
        .then_ignore(punct_plus_p().then(punct_equal_p()))
        .then(expression_parser.clone())
        .then_ignore(punct_semicolon_p())
        .map(|(l, r)| StatementKind::AddAssign(l, r));
      let sub_assign = expression_parser
        .clone()
        .then_ignore(punct_minus_p().then(punct_equal_p()))
        .then(expression_parser.clone())
        .then_ignore(punct_semicolon_p())
        .map(|(l, r)| StatementKind::SubAssign(l, r));
      let mul_assign = expression_parser
        .clone()
        .then_ignore(punct_asterisk_p().then(punct_equal_p()))
        .then(expression_parser.clone())
        .then_ignore(punct_semicolon_p())
        .map(|(l, r)| StatementKind::MulAssign(l, r));
      let div_assign = expression_parser
        .clone()
        .then_ignore(punct_slash_p().then(punct_equal_p()))
        .then(expression_parser.clone())
        .then_ignore(punct_semicolon_p())
        .map(|(l, r)| StatementKind::DivAssign(l, r));
      let mod_assign = expression_parser
        .clone()
        .then_ignore(punct_percent_p().then(punct_equal_p()))
        .then(expression_parser.clone())
        .then_ignore(punct_semicolon_p())
        .map(|(l, r)| StatementKind::ModAssign(l, r));
      let shl_assign = expression_parser
        .clone()
        .then_ignore(shl_p().then(punct_equal_p()))
        .then(expression_parser.clone())
        .then_ignore(punct_semicolon_p())
        .map(|(l, r)| StatementKind::ShiftLeftAssign(l, r));
      let shr_assign = expression_parser
        .clone()
        .then_ignore(shr_p().then(punct_equal_p()))
        .then(expression_parser.clone())
        .then_ignore(punct_semicolon_p())
        .map(|(l, r)| StatementKind::ShiftRightAssign(l, r));
      let bitand_assign = expression_parser
        .clone()
        .then_ignore(punct_ampersand_p().then(punct_equal_p()))
        .then(expression_parser.clone())
        .then_ignore(punct_semicolon_p())
        .map(|(l, r)| StatementKind::BitAndAssign(l, r));
      let bitor_assign = expression_parser
        .clone()
        .then_ignore(punct_pipe_p().then(punct_equal_p()))
        .then(expression_parser.clone())
        .then_ignore(punct_semicolon_p())
        .map(|(l, r)| StatementKind::BitOrAssign(l, r));
      let bitxor_assign = expression_parser
        .clone()
        .then_ignore(punct_caret_p().then(punct_equal_p()))
        .then(expression_parser.clone())
        .then_ignore(punct_semicolon_p())
        .map(|(l, r)| StatementKind::BitXorAssign(l, r));
      //

      let statement_body_p = statement_parser
        .clone()
        .then_ignore(punct_semicolon_p())
        .repeated()
        .collect::<Vec<_>>()
        .then(expression_parser.clone().or_not())
        .nested_in(braces_content_p())
        .map(|(body, last_expr)| StatementBody { body, last_expr })
        .labelled("statement_body2")
        .as_context();
      let times_kw = StrID::from("times");
      let loop_times = punct_quote_p()
        .ignore_then(spanned_ident_p())
        .then_ignore(punct_colon_p())
        .or_not()
        .then_ignore(kw_loop_p())
        .then(choice((ident_p(), num_lit_p())).map_with(|i, ex| (i, ex.span())))
        .then_ignore(ident_p().filter(move |str_id| *str_id == times_kw))
        .then(statement_body_p.clone())
        .map_with(|((name, (times, times_span)), steps), ex| {
          StatementKind::ExprStmt(Expr {
            span: ex.span(),
            kind: Box::new(ExprKind::LoopTimes(ExprLoopTimes {
              name,
              times,
              times_span,
              steps,
            })),
          })
        })
        .labelled("loop_times_statement")
        .as_context();
      let expr_stmt = expression_parser
        .clone()
        .then_ignore(punct_semicolon_p())
        .map(StatementKind::ExprStmt);

      choice((
        let_,
        assign,
        add_assign,
        sub_assign,
        mul_assign,
        div_assign,
        mod_assign,
        shl_assign,
        shr_assign,
        bitand_assign,
        bitor_assign,
        bitxor_assign,
        loop_times,
        expr_stmt,
      ))
    };
    attributes.then(kind).map_with(|(the_attributes, kind), ex| Statement {
      span: ex.span(),
      attributes: if the_attributes.is_empty() {
        None
      } else {
        Some(Box::new(the_attributes))
      },
      kind: Box::new(kind),
    })
  });
}

pub fn recursive_parser_group_p<'src>() -> (
  impl YagParser<'src, Expr>,
  impl YagParser<'src, Expr>,
  impl YagParser<'src, Statement>,
) {
  let mut expression_parser = Recursive::declare();
  let mut condition_parser = Recursive::declare();
  let mut statement_parser = Recursive::declare();
  define_expression_parser(
    &mut expression_parser,
    &mut condition_parser,
    &mut statement_parser,
    false,
  );
  define_expression_parser(
    &mut expression_parser,
    &mut condition_parser,
    &mut statement_parser,
    true,
  );
  define_statement_parser(
    &mut expression_parser,
    &mut condition_parser,
    &mut statement_parser,
  );
  (expression_parser, condition_parser, statement_parser)
}

pub fn attributes_p<'src>() -> impl YagParser<'src, Vec<Expr>> {
  punct_hash_p()
    .ignore_then(expr_p().nested_in(brackets_content_p()))
    .labelled("attribute")
    .as_context()
    .repeated()
    .collect::<Vec<_>>()
}

pub fn bitbag_p<'src>() -> impl YagParser<'src, AstItem> {
  let field_def_p = attributes_p()
    .then(spanned_ident_p())
    .then_ignore(punct_colon_p())
    .then(expr_p().map_with(|bit, ex| (bit, ex.span())))
    .map_with(|(((attributes, (name, name_span)), (bit, bit_span))), ex| {
      AstBitbagFieldDef {
        span: ex.span(),
        attributes,
        name,
        name_span,
        bit,
        bit_span,
      }
    });
  attributes_p()
    .then_ignore(kw_bitbag_p())
    .then(spanned_ident_p())
    .then(
      field_def_p
        .separated_by(punct_comma_p())
        .allow_trailing()
        .collect::<Vec<_>>()
        .nested_in(braces_content_p()),
    )
    .map_with(|((attributes, (name, name_span)), fields), ex| AstItem {
      file_id: ex.state().file_id,
      attributes,
      name,
      name_span,
      span: ex.span(),
      kind: AstItemKind::Bitbag(AstBitbag { fields }),
    })
    .labelled("bitbag_definition")
    .as_context()
}

pub fn struct_p<'src>() -> impl YagParser<'src, AstItem> {
  let field_def_p =
    attributes_p()
      .then(spanned_ident_p())
      .then_ignore(punct_colon_p())
      .then(type_name_p())
      .map_with(|(((attributes, (name, name_span)), ty)), ex| {
        AstStructFieldDef { span: ex.span(), attributes, name, name_span, ty }
      })
      .labelled("struct_field")
      .as_context();
  attributes_p()
    .then_ignore(kw_struct_p())
    .then(spanned_ident_p())
    .then(
      field_def_p
        .separated_by(punct_comma_p())
        .allow_trailing()
        .collect::<Vec<_>>()
        .nested_in(braces_content_p()),
    )
    .map_with(|((attributes, (name, name_span)), fields), ex| AstItem {
      file_id: ex.state().file_id,
      attributes,
      name,
      name_span,
      span: ex.span(),
      kind: AstItemKind::Struct(AstStruct { fields }),
    })
    .labelled("struct_definition")
    .as_context()
}

pub fn const_p<'src>() -> impl YagParser<'src, AstItem> {
  attributes_p()
    .then_ignore(kw_const_p())
    .then(spanned_ident_p())
    .then_ignore(punct_colon_p())
    .then(type_name_p())
    .then_ignore(punct_equal_p())
    .then(expr_p())
    .then_ignore(punct_semicolon_p())
    .map_with(|(((attributes, (name, name_span)), ty), expr), ex| AstItem {
      file_id: ex.state().file_id,
      attributes,
      name,
      name_span,
      span: ex.span(),
      kind: AstItemKind::Const(AstConst { ty, expr }),
    })
    .labelled("const_definition")
    .as_context()
}

pub fn static_p<'src>() -> impl YagParser<'src, AstItem> {
  let rom_kind = attributes_p()
    .then_ignore(kw_static_p())
    .then_ignore(kw_rom_p())
    .then(spanned_ident_p())
    .then_ignore(punct_colon_p())
    .then(type_name_p())
    .then_ignore(punct_equal_p())
    .then(expr_p())
    .then_ignore(punct_semicolon_p())
    .map_with(|(((attributes, (name, name_span)), ty), expr), ex| AstItem {
      file_id: ex.state().file_id,
      attributes,
      name,
      name_span,
      span: ex.span(),
      kind: AstItemKind::Static(AstStatic {
        ty,
        kind: AstStaticKind::Rom(expr),
      }),
    });
  let ram_kind = attributes_p()
    .then_ignore(kw_static_p())
    .then_ignore(kw_ram_p())
    .then(spanned_ident_p())
    .then_ignore(punct_colon_p())
    .then(type_name_p())
    .then_ignore(punct_equal_p())
    .then(expr_p())
    .then_ignore(punct_semicolon_p())
    .map_with(|(((attributes, (name, name_span)), ty), expr), ex| AstItem {
      file_id: ex.state().file_id,
      attributes,
      name,
      name_span,
      span: ex.span(),
      kind: AstItemKind::Static(AstStatic {
        ty,
        kind: AstStaticKind::Ram(expr),
      }),
    });
  let mmio_kind = attributes_p()
    .then_ignore(kw_static_p())
    .then_ignore(kw_mmio_p())
    .then(spanned_ident_p())
    .then_ignore(punct_colon_p())
    .then(type_name_p())
    .then_ignore(punct_semicolon_p())
    .map_with(|((attributes, (name, name_span)), ty), ex| AstItem {
      file_id: ex.state().file_id,
      attributes,
      name,
      name_span,
      span: ex.span(),
      kind: AstItemKind::Static(AstStatic {
        ty,
        kind: AstStaticKind::MemoryMappedIO,
      }),
    });
  choice((rom_kind, ram_kind, mmio_kind))
    .labelled("static_definition")
    .as_context()
}

pub fn function_p<'src>() -> impl YagParser<'src, AstItem> {
  let function_arg_p = attributes_p()
    .then(spanned_ident_p())
    .then_ignore(punct_colon_p())
    .then(type_name_p())
    .map_with(|(((attributes, (name, name_span)), ty)), ex| AstFunctionArg {
      span: ex.span(),
      attributes,
      name,
      name_span,
      ty,
    });
  let statement_recovery = attributes_p()
    .or_not()
    .then_ignore(none_of(Lone(Semicolon)).repeated())
    .then_ignore(just(Lone(Semicolon)))
    .map_with(|attributes, ex| Statement {
      attributes: attributes.map(|a| Box::new(a)),
      kind: Box::new(StatementKind::StatementKindError),
      span: ex.span(),
    });

  attributes_p()
    .then_ignore(kw_fn_p())
    .then(spanned_ident_p())
    .then(
      function_arg_p
        .separated_by(punct_comma_p())
        .allow_trailing()
        .collect::<Vec<_>>()
        .nested_in(parens_content_p()),
    )
    .then(
      punct_minus_p()
        .then_ignore(punct_greater_than_p())
        .ignore_then(spanned_ident_p())
        .or_not(),
    )
    .then(
      statement_p()
        .recover_with(via_parser(statement_recovery))
        .repeated()
        .collect::<Vec<_>>()
        .then(expr_p().or_not())
        .nested_in(braces_content_p())
        .map(|(body, last_expr)| StatementBody { body, last_expr }),
    )
    .map_with(
      |((((attributes, (name, name_span)), args), return_info), body), ex| {
        AstItem {
          file_id: ex.state().file_id,
          attributes,
          name,
          name_span,
          span: ex.span(),
          kind: AstItemKind::Function(AstFunction { args, return_info, body }),
        }
      },
    )
    .labelled("function_definition")
    .as_context()
}

pub fn item_p<'src>() -> impl YagParser<'src, AstItem> {
  choice((bitbag_p(), struct_p(), const_p(), static_p(), function_p()))
    .labelled("item_definition")
    .as_context()
}

pub fn type_name_p<'src>() -> impl YagParser<'src, TypeName> {
  recursive(|type_name_parser| {
    let ident = ident_p().map(|i| TypeNameKind::Ident(i));
    let array_num_lit = type_name_parser
      .clone()
      .then_ignore(punct_semicolon_p())
      .then(num_lit_p().map_with(|n, ex| (n, ex.span())))
      .nested_in(brackets_content_p())
      .map(|(elem, (count, count_span))| {
        TypeNameKind::ArrayNumLit(Box::new(elem), count, count_span)
      });
    let array_const_name = type_name_parser
      .clone()
      .then_ignore(punct_semicolon_p())
      .then(ident_p().map_with(|n, ex| (n, ex.span())))
      .nested_in(brackets_content_p())
      .map(|(elem, (count, count_span))| {
        TypeNameKind::ArrayConstName(Box::new(elem), count, count_span)
      });
    let kind = choice((ident, array_num_lit, array_const_name));
    kind.map_with(|kind, ex| TypeName { kind, span: ex.span() })
  })
  .labelled("type_name")
  .as_context()
}

pub fn items_of<'src>(
  trees: &'src [(TokenTree, Span32)], yag_state: YagParserState,
) -> (Vec<AstItem>, Vec<Rich<'src, TokenTree, Span32>>) {
  let eoi: Span32 = match trees.last() {
    Some(s) => s.1,
    None => return (Vec::new(), Vec::new()),
  };
  let mut simple_state = SimpleState(yag_state);

  let item_keywords =
    [Lone(KwBitbag), Lone(KwStruct), Lone(KwConst), Lone(KwStatic), Lone(KwFn)];
  let recovery = attributes_p()
    .then(
      one_of(item_keywords.clone())
        .then(none_of(item_keywords.clone()).repeated())
        .ignored(),
    )
    .map_with(|(attributes, ()), ex| AstItem {
      file_id: ex.state().file_id,
      attributes,
      name: StrID::default(),
      name_span: ex.span(),
      span: ex.span(),
      kind: AstItemKind::ItemKindError,
    });

  let items_parser =
    item_p().recover_with(via_parser(recovery)).repeated().collect::<Vec<_>>();

  let (opt_out, errors): (Option<Vec<AstItem>>, Vec<_>) = items_parser
    .parse_with_state(
      Input::map(trees, eoi, |(tk, span)| (tk, span)),
      &mut simple_state,
    )
    .into_output_errors();

  (opt_out.unwrap_or_default(), errors)
}
