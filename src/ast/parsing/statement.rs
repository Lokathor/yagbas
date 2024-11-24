use super::*;

/// Parses [TokenTree] into specifically a [Statement]
pub fn statement_p<'src, I, M>(
  make_input: M,
) -> impl Parser<'src, I, Statement, ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
  M: Fn(&'src [FileSpanned<TokenTree>], FileSpan) -> I + Copy + 'src,
{
  recursive(|statements| {
    let loop_stmt = {
      let opt_name = quote_p()
        .ignore_then(ident_p())
        .then_ignore(colon_p())
        .or_not()
        .labelled("loop_name")
        .as_context();
      let keyword = kw_loop_p();
      let loop_body = statements
        .clone()
        .map_with(|tts, e| FileSpanned::new(tts, e.span()))
        .separated_by(statement_sep_p().repeated().at_least(1))
        .allow_leading()
        .allow_trailing()
        .collect()
        .nested_in(nested_brace_content_p(make_input))
        .labelled("loop_body")
        .as_context();
      let x = opt_name
        .then_ignore(keyword)
        .then(loop_body)
        .map(|(opt_name, body)| {
          let name = opt_name.unwrap_or_default();
          Statement::Loop(Loop::new_with_name(name, body))
        })
        .labelled("loop_stmt")
        .as_context();
      x
    };

    let continue_stmt = {
      let keyword = kw_continue_p();
      let opt_name = quote_p()
        .ignore_then(ident_p())
        .or_not()
        .labelled("continue_target")
        .as_context();
      let x = keyword
        .ignore_then(opt_name)
        .map(|opt_n| Statement::Continue(opt_n.unwrap_or_default()))
        .labelled("continue_stmt")
        .as_context();
      x
    };

    let break_stmt = {
      let keyword = kw_break_p();
      let opt_name = quote_p()
        .ignore_then(ident_p())
        .or_not()
        .labelled("break_target")
        .as_context();
      let x = keyword
        .ignore_then(opt_name)
        .map(|opt_n| Statement::Break(opt_n.unwrap_or_default()))
        .labelled("break_stmt")
        .as_context();
      x
    };

    let call = ident_p()
      .map_with(|i, extras| FileSpanned::new(i, extras.span()))
      .then(parenthesis_p())
      .map(|(target, args)| Statement::Call { target, args })
      .labelled("call_stmt")
      .as_context();

    let assign8_const = reg8_p()
      .map_with(|r, extras| FileSpanned::new(r, extras.span()))
      .then_ignore(equal_p())
      .then(const_expr_p(make_input))
      .map(|(target, value)| Statement::AssignReg8Const { target, value })
      .labelled("assign8_const")
      .as_context();

    let x = choice((
      kw_return_p(),
      call,
      loop_stmt,
      continue_stmt,
      break_stmt,
      assign8_const,
    ));

    x
  })
}
