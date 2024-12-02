use chumsky::recovery::ViaParser;

use super::*;

/// Parses a single statement, including all inner statements.
pub fn statement_p<'src, I, M>(
  make_input: M,
) -> impl Parser<'src, I, FileSpanned<Statement>, ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
  M: Fn(&'src [FileSpanned<TokenTree>], FileSpan) -> I + Copy + 'src,
{
  recursive(|stmt| {
    let expr = expression_p(make_input)
      .map(Statement::Expression)
      .labelled("expression_statement")
      .as_context();

    choice((expr,))
  })
  .map_with(FileSpanned::from_extras)
  .labelled("statement")
  .as_context()
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

/// This is a macro instead of a function because I can't figure out what type
/// signature to put on this expression so that Rust lets me actually use the
/// darn thing.
#[macro_export]
macro_rules! statement_recovery_strategy {
  () => {
    via_parser(
      any().and_is(statement_sep_p().not()).repeated().at_least(1).map_with(
        |_, extras| FileSpanned::from_extras(Statement::StatementError, extras),
      ),
    )
  };
}
