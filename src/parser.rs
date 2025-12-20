use crate::Token::*;
use crate::TokenTree::*;
use crate::*;
use chumsky::{input::MappedInput, prelude::*, recursive::Indirect};

#[allow(unused_macros)]
macro_rules! infix_maker {
  ($kind: path) => {
    |lhs, _op, rhs, extras| Expr {
      span: extras.span(),
      kind: Box::new(ExprKind::BinOp(ExprBinOp { lhs, rhs, kind: $kind })),
    }
  };
}
#[allow(unused_macros)]
macro_rules! prefix_maker {
  ($kind: path) => {
    |_op, inner, extras| Expr {
      span: extras.span(),
      kind: Box::new(ExprKind::UnOp(ExprUnOp { inner, kind: $kind })),
    }
  };
}

pub fn items_of<'src>(
  trees: &'src [(TokenTree, Span32)], yag_state: YagParserState,
) -> (Vec<AstItem>, Vec<Rich<'src, TokenTree, Span32>>) {
  let eoi: Span32 = match trees.last() {
    Some(s) => s.1,
    None => return (Vec::new(), Vec::new()),
  };
  let recovery =
    via_parser(any().repeated().at_least(1).map_with(|_, ex| AstItem {
      attributes: Vec::new(),
      file_id: yag_state.file_id,
      span: ex.span(),
      kind: AstItemKind::AstItemKindError,
    }));

  let parser = item_p().recover_with(recovery).repeated().collect::<Vec<_>>();
  let mut state = SimpleState(yag_state);

  let (opt_out, errors): (Option<Vec<AstItem>>, Vec<_>) = parser
    .parse_with_state(
      Input::map(trees, eoi, |(tk, span)| (tk, span)),
      &mut state,
    )
    .into_output_errors();

  (opt_out.unwrap_or_default(), errors)
}

pub fn item_p<'src>() -> impl YagParser<'src, AstItem> {
  let mut attributes_p = Recursive::declare();
  let mut statement_p = Recursive::declare();
  let mut expr_p = Recursive::declare();

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
      .then_ignore(punct_semicolon_p())
      .map(|(((name, name_span), opt_ty), opt_init)| {
        StatementKind::Let(name, name_span, opt_ty, opt_init)
      });
    let assign_kind = expr_p
      .clone()
      .then_ignore(punct_equal_p())
      .then(expr_p.clone())
      .then_ignore(punct_semicolon_p())
      .map(|(lhs, rhs)| StatementKind::Assign(lhs, rhs));
    let bin_op_kind = expr_p
      .clone()
      .then(bin_op_assign_p())
      .then(expr_p.clone())
      .map(|((lhs, bin), rhs)| StatementKind::BinOpAssign(lhs, bin, rhs));

    attributes_p
      .clone()
      .then(
        choice((let_kind, assign_kind, bin_op_kind)).recover_with(via_parser(
          punct_semicolon_p()
            .not()
            .repeated()
            .then(punct_semicolon_p())
            .to(StatementKind::StatementKindError),
        )),
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
  });

  chumsky::prelude::todo()
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
  chumsky::prelude::todo()
}
