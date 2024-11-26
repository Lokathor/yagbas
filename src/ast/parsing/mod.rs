use crate::{
  ast::{Function, Item, Loop, Reg8, Statement},
  src_files::{FileSpan, FileSpanned},
  str_id::StrID,
};
use chumsky::{
  extra::Err,
  input::{BorrowInput, ValueInput},
  prelude::*,
};

use super::{
  const_expr::ConstExpr,
  token::Token::{self, *},
  token_tree::TokenTree::{self, *},
  NamedConst,
};

pub type ErrRichToken<'src> = Err<Rich<'src, Token, FileSpan>>;
pub type ErrRichTokenTree<'src> = Err<Rich<'src, TokenTree, FileSpan>>;

mod token_tree;
pub use token_tree::*;

mod statement_parser;
pub use statement_parser::*;

mod compare_test_parser;
pub use compare_test_parser::*;

mod lone_tokens;
pub use lone_tokens::*;

mod static_parser;
pub use static_parser::*;

/// Parses [TokenTree] into any [Item]
pub fn item_p<'src, I, M>(
  make_input: M,
) -> impl Parser<'src, I, Item, ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
  M: Fn(&'src [FileSpanned<TokenTree>], FileSpan) -> I + Copy + 'src,
{
  let function = function_p(make_input).map(Item::Function);
  let named_const = named_const_p(make_input).map(Item::NamedConst);
  let static_ = static_p(make_input).map(Item::Static);

  let x = choice((function, named_const, static_));

  x
}

/// Parses [TokenTree] into specifically a [Function]
pub fn function_p<'src, I, M>(
  make_input: M,
) -> impl Parser<'src, I, Function, ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
  M: Fn(&'src [FileSpanned<TokenTree>], FileSpan) -> I + Copy + 'src,
{
  let name = ident_p()
    .map_with(|i, e| FileSpanned::new(i, e.span()))
    .labelled("fn_name")
    .as_context();
  let args = parenthesis_p().labelled("fn_arg_parens").as_context();
  let statement_recover_strategy = via_parser(
    any()
      .and_is(statement_sep_p().not())
      .repeated()
      .at_least(1)
      .to(Statement::StatementError),
  );
  let fn_body = statement_p(make_input)
    .recover_with(statement_recover_strategy)
    .map_with(|s: Statement, e| FileSpanned::new(s, e.span()))
    .separated_by(statement_sep_p().repeated().at_least(1))
    .allow_leading()
    .allow_trailing()
    .collect()
    .nested_in(nested_brace_content_p(make_input))
    .labelled("fn_body")
    .as_context();
  // Note(Lokathor): This stupid thing is because RA is weird sometimes.
  // https://github.com/rust-lang/rust-analyzer/issues/18542
  let x = Parser::map(
    kw_fn_p().ignore_then(name).then(args).then(fn_body),
    |((name, arguments), statements)| Function { name, arguments, statements },
  )
  .labelled("function")
  .as_context();

  x
}

/// Parses [TokenTree] into specifically a [NamedConst]
pub fn named_const_p<'src, I, M>(
  make_input: M,
) -> impl Parser<'src, I, NamedConst, ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
  M: Fn(&'src [FileSpanned<TokenTree>], FileSpan) -> I + Copy + 'src,
{
  let name = ident_p()
    .map_with(|i, e| FileSpanned::new(i, e.span()))
    .labelled("const_name")
    .as_context();
  let expr = const_expr_p(make_input).labelled("const_expression").as_context();
  // Note(Lokathor): This stupid thing is because RA is weird sometimes.
  // https://github.com/rust-lang/rust-analyzer/issues/18542
  let x = Parser::map(
    kw_const_p()
      .ignore_then(name)
      .then_ignore(equal_p())
      .then(expr)
      .then_ignore(statement_sep_p()),
    |(name, expr)| NamedConst { name, expr },
  )
  .labelled("named_const")
  .as_context();

  x
}

/// Parses a single constant expression.
pub fn const_expr_p<'src, I, M>(
  make_input: M,
) -> impl Parser<'src, I, FileSpanned<ConstExpr>, ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
  M: Fn(&'src [FileSpanned<TokenTree>], FileSpan) -> I + Copy + 'src,
{
  use chumsky::pratt::*;

  recursive(|expr| {
    let atom = {
      let num_lit = num_lit_p().map_with(|n, extras| {
        FileSpanned::new(ConstExpr::Literal(n), extras.span())
      });
      let ident = ident_p().map_with(|i, extras| {
        FileSpanned::new(ConstExpr::Ident(i), extras.span())
      });
      let parens = expr.nested_in(nested_parens_content_p(make_input));

      choice((num_lit, ident, parens))
    };

    let with_pratt = atom.pratt((
      prefix(13, minus_p(), |_op, x, extra| {
        FileSpanned::new(ConstExpr::Neg(Box::new(x)), extra.span())
      }),
      infix(left(10), plus_p(), |l, _op, r, extra| {
        FileSpanned::new(ConstExpr::Add(Box::new(l), Box::new(r)), extra.span())
      }),
      infix(left(10), minus_p(), |l, _op, r, extra| {
        FileSpanned::new(ConstExpr::Sub(Box::new(l), Box::new(r)), extra.span())
      }),
    ));

    with_pratt
  })
}

/// Parses a `Lone(Newline)` or `Lone(Semicolon)`, which is then discarded.
pub fn statement_sep_p<'src, I>(
) -> impl Parser<'src, I, (), ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
{
  select! {
    Lone(Newline) => (),
    Lone(Semicolon) => (),
  }
}

/// Parses an 8-bit register keyword and returns it.
pub fn reg8_p<'src, I>(
) -> impl Parser<'src, I, Reg8, ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
{
  select! {
    Lone(KwA) => Reg8::A,
    Lone(KwB) => Reg8::B,
    Lone(KwC) => Reg8::C,
    Lone(KwD) => Reg8::D,
    Lone(KwE) => Reg8::E,
    Lone(KwH) => Reg8::H,
    Lone(KwL) => Reg8::L,
  }
}

/// Parses `Parens(p)` and returns `p`.
pub fn parenthesis_p<'src, I>(
) -> impl Parser<'src, I, Vec<FileSpanned<TokenTree>>, ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
{
  select! {
    Parens(p) = ex => p,
  }
}

/// Parses `Braces(p)` and returns `p`.
pub fn braces_p<'src, I>(
) -> impl Parser<'src, I, Vec<FileSpanned<TokenTree>>, ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
{
  select! {
    Braces(p) = ex => p,
  }
}

/// Lets you `select_ref!` the content out of some `Braces`
pub fn nested_brace_content_p<'src, I, M>(
  make_input: M,
) -> impl Parser<'src, I, I, ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
  M: Fn(&'src [FileSpanned<TokenTree>], FileSpan) -> I + Copy + 'src,
{
  select_ref! {
    Braces(b) = ex => make_input(b, ex.span()),
  }
}

/// Lets you `select_ref!` the content out of some `Brackets`
pub fn nested_bracket_content_p<'src, I, M>(
  make_input: M,
) -> impl Parser<'src, I, I, ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
  M: Fn(&'src [FileSpanned<TokenTree>], FileSpan) -> I + Copy + 'src,
{
  select_ref! {
    Brackets(b) = ex => make_input(b, ex.span()),
  }
}

/// Lets you `select_ref!` the content out of some `Parens`
pub fn nested_parens_content_p<'src, I, M>(
  make_input: M,
) -> impl Parser<'src, I, I, ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
  M: Fn(&'src [FileSpanned<TokenTree>], FileSpan) -> I + Copy + 'src,
{
  select_ref! {
    Parens(b) = ex => make_input(b, ex.span()),
  }
}
