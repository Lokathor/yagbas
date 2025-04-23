use super::*;

/// Any of the things that can go in a body of code.
#[derive(Debug, Clone)]
pub enum Statement {
  Expr(Expr),
  IfElse(Box<IfElse>),
  Loop(Box<Loop>),
  Break(Option<StrID>),
  Continue(Option<StrID>),
  Call(StrID),
  Return,
  StatementError,
}

/// Parse one [Statement]
pub(crate) fn statement_p<'src, I, M>(
  make_input: M,
) -> impl Parser<'src, I, Statement, AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
  M: Fn(&'src [(TokenTree, SimpleSpan)], SimpleSpan) -> I + Copy + 'src,
{
  let expr = expr_p(make_input).map(|S(expr, _span)| Statement::Expr(expr));
  let ret = select! {TokenTree::Lone(Token::KwReturn) => Statement::Return};

  choice((expr, ret))
}

/// Parse and discard a separator between two statements.
fn statement_sep_p<'src, I>()
-> impl Parser<'src, I, (), AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  select! {
    TokenTree::Lone(Token::Newline) => (),
    TokenTree::Lone(Token::Semicolon) => (),
  }
}

/// Parse many statements that are grouped inside braces
pub(crate) fn statement_body_p<'src, I, M>(
  make_input: M,
) -> impl Parser<'src, I, Vec<S<Statement>>, AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
  M: Fn(&'src [(TokenTree, SimpleSpan)], SimpleSpan) -> I + Copy + 'src,
{
  let one_statement = statement_p(make_input)
    .recover_with(statement_recovery_strategy!())
    .map_with(|t, ex| S::from_extras(t, ex));

  let separator = statement_sep_p().repeated().at_least(1);

  let many_statements = one_statement
    .separated_by(separator)
    .allow_leading()
    .allow_trailing()
    .collect();

  many_statements.nested_in(braces_content_p(make_input))
}

/// This is a macro instead of a function because I can't figure out what type
/// signature to put on this expression so that Rust lets me actually use the
/// darn thing in more than one place.
#[macro_export]
macro_rules! statement_recovery_strategy {
  () => {
    via_parser(
      any()
        .and_is(statement_sep_p().not())
        .repeated()
        .at_least(1)
        .to(Statement::StatementError),
    )
  };
}
