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
    let braced_statements = stmt
      .clone()
      .recover_with(statement_recovery_strategy!())
      .separated_by(statement_sep_p().repeated().at_least(1))
      .allow_leading()
      .allow_trailing()
      .collect()
      .nested_in(braces_content_p(make_input));

    let expr = expression_p(make_input)
      .map(Statement::Expression)
      .labelled("expression_statement")
      .as_context();
    let loop_ = {
      let name = quote_p()
        .ignore_then(ident_p())
        .then_ignore(colon_p())
        .or_not()
        .map_with(FileSpanned::from_extras);
      let kw = kw_loop_p();
      let body = braced_statements.clone().labelled("loop_body").as_context();
      name
        .then_ignore(kw)
        .then(body)
        .map_with(|(name, statements), extras| {
          Statement::Loop(FileSpanned::from_extras(
            Loop::new(name, statements),
            extras,
          ))
        })
        .labelled("loop_statement")
        .as_context()
    };
    let return_ = kw_return_p()
      .to(Statement::Return)
      .labelled("return_statement")
      .as_context();
    let break_ = kw_break_p()
      .ignore_then(
        quote_p()
          .ignore_then(ident_p())
          .or_not()
          .map_with(FileSpanned::from_extras),
      )
      .map(Statement::Break)
      .labelled("break_statement")
      .as_context();
    let continue_ = kw_continue_p()
      .ignore_then(
        quote_p()
          .ignore_then(ident_p())
          .or_not()
          .map_with(FileSpanned::from_extras),
      )
      .map(Statement::Continue)
      .labelled("continue_statement")
      .as_context();
    let call = ident_p()
      .then(
        select! {
          Parens(p) = ex => p,
        }
        .labelled("call_args")
        .as_context(),
      )
      .map_with(|(target, args), extras| {
        Statement::Call(FileSpanned::from_extras(Call { target, args }, extras))
      });
    let if_else = {
      let if_body = braced_statements.clone().labelled("if_body").as_context();
      let else_body =
        braced_statements.clone().labelled("else_body").as_context();

      kw_if_p()
        .ignore_then(expression_p(make_input))
        .then(if_body)
        .then(
          newline_p()
            .repeated()
            .ignore_then(kw_else_p())
            .ignore_then(else_body)
            .or_not(),
        )
        .map_with(|((test, if_body), else_body), extras| {
          Statement::IfElse(FileSpanned::from_extras(
            IfElse::new(test, if_body, else_body.unwrap_or_default()),
            extras,
          ))
        })
    };

    // Note(Lokathor): I'm not sure if it's significant that we try to parse a
    // call before an expression, but they do both start with an `ident`, so
    choice((call, expr, loop_, return_, break_, continue_, if_else))
      .map_with(FileSpanned::from_extras)
  })
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
