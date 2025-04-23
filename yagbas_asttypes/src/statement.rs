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
  recursive(|stmt| {
    let braced_statements = stmt
      .clone()
      .recover_with(statement_recovery_strategy!())
      .map_with(S::from_extras)
      .separated_by(statement_sep_p().repeated().at_least(1))
      .allow_leading()
      .allow_trailing()
      .collect()
      .nested_in(braces_content_p(make_input));

    let expr = expr_p(make_input).map(|S(expr, _span)| Statement::Expr(expr));
    let loop_ = {
      let name = quote_p()
        .ignore_then(ident_p())
        .then_ignore(colon_p())
        .or_not()
        .map_with(S::from_extras)
        .labelled("loop_label")
        .as_context();
      let kw = kw_loop_p();
      let body = braced_statements.clone().labelled("loop_body").as_context();
      name.then_ignore(kw).then(body).map(|(opt_name, body)| {
        Statement::Loop(Box::new(Loop { opt_name, body }))
      })
    };
    let return_ = kw_return_p().to(Statement::Return);
    let break_ = kw_break_p()
      .ignore_then(quote_p().ignore_then(ident_p()).or_not())
      .map(Statement::Break);
    let continue_ = kw_continue_p()
      .ignore_then(quote_p().ignore_then(ident_p()).or_not())
      .map(Statement::Continue);
    let call = ident_p().then_ignore(parens_p()).map(Statement::Call);
    let if_else = {
      let if_body = braced_statements.clone();
      let else_body = braced_statements.clone();

      kw_if_p()
        .ignore_then(expr_p(make_input))
        .then(if_body)
        .then(
          newline_p()
            .repeated()
            .ignore_then(kw_else_p())
            .ignore_then(else_body)
            .or_not(),
        )
        .map(|((condition, if_body), else_body)| {
          Statement::IfElse(Box::new(IfElse {
            condition,
            if_body,
            else_body: else_body.unwrap_or_default(),
          }))
        })
    };

    // Note(Lokathor): I'm not sure if it's significant that we try to parse a
    // call before an expression, but they do both start with an `ident`, so
    choice((call, expr, loop_, return_, break_, continue_, if_else))
  })
}

/// Branching construct.
#[derive(Debug, Clone)]
pub struct IfElse {
  pub condition: S<Expr>,
  pub if_body: Vec<S<Statement>>,
  pub else_body: Vec<S<Statement>>,
}

/// Repeating code construct.
#[derive(Debug, Clone)]
pub struct Loop {
  pub opt_name: S<Option<StrID>>,
  pub body: Vec<S<Statement>>,
}

/// Parse and discard a separator between two statements.
pub(crate) fn statement_sep_p<'src, I>()
-> impl Parser<'src, I, (), AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  select! {
    TokenTree::Lone(Token::Newline) => (),
    TokenTree::Lone(Token::Semicolon) => (),
  }
}
