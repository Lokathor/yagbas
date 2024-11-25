use crate::ast::{CompareTest, IfElse};

use super::*;

/// Parses [TokenTree] into specifically a [Statement]
pub fn statement_p<'src, I, M>(
  make_input: M,
) -> impl Parser<'src, I, Statement, ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
  M: Fn(&'src [FileSpanned<TokenTree>], FileSpan) -> I + Copy + 'src,
{
  let statement_recover_strategy = via_parser(
    any()
      .and_is(statement_sep_p().not())
      .repeated()
      .at_least(1)
      .to(Statement::StatementError),
  );
  let if_test_recover_strategy = via_parser(
    any()
      .and_is(braces_p::<'_, I>().not())
      .repeated()
      .to(CompareTest::CompareTestError),
  );

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
        .recover_with(statement_recover_strategy.clone())
        .map_with(|tts, e| FileSpanned::new(tts, e.span()))
        .separated_by(statement_sep_p().repeated().at_least(1))
        .allow_leading()
        .allow_trailing()
        .collect()
        .nested_in(nested_brace_content_p(make_input))
        .labelled("loop_body")
        .as_context();
      let x = Parser::map(
        opt_name.then_ignore(keyword).then(loop_body),
        |(opt_name, body)| {
          let name = opt_name.unwrap_or_default();
          Statement::Loop(Loop::new_with_name(name, body))
        },
      )
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

    let load_reg8_const = reg8_p()
      .map_with(|r, extras| FileSpanned::new(r, extras.span()))
      .then_ignore(equal_p())
      .then(const_expr_p(make_input))
      .map(|(reg8, expr)| Statement::LoadReg8Const { reg8, expr })
      .labelled("load_reg8_const")
      .as_context();

    let store_a_to_const_addr = const_expr_p(make_input)
      .nested_in(nested_bracket_content_p(make_input))
      .then_ignore(equal_p())
      .then_ignore(kw_a_p())
      .map(Statement::StoreAToConstAddress)
      .labelled("store_a_to_const_addr")
      .as_context();

    let load_a_from_const_addr = kw_a_p()
      .ignore_then(equal_p())
      .ignore_then(
        const_expr_p(make_input)
          .nested_in(nested_bracket_content_p(make_input)),
      )
      .map(Statement::LoadAFromConstAddress)
      .labelled("load_a_from_const_addr")
      .as_context();

    let if_else = {
      let if_test = compare_test_p(make_input)
        .recover_with(if_test_recover_strategy)
        .map_with(|x, e| FileSpanned::new(x, e.span()))
        .labelled("if_test")
        .as_context();
      let if_body = statements
        .clone()
        .recover_with(statement_recover_strategy.clone())
        .map_with(|statements, e| FileSpanned::new(statements, e.span()))
        .separated_by(statement_sep_p().repeated().at_least(1))
        .allow_leading()
        .allow_trailing()
        .collect()
        .nested_in(nested_brace_content_p(make_input))
        .labelled("if_body")
        .as_context();
      let else_body = statements
        .clone()
        .recover_with(statement_recover_strategy.clone())
        .map_with(|statements, e| FileSpanned::new(statements, e.span()))
        .separated_by(statement_sep_p().repeated().at_least(1))
        .allow_leading()
        .allow_trailing()
        .collect()
        .nested_in(nested_brace_content_p(make_input))
        .labelled("else_body")
        .as_context();
      let if_else_parser = kw_if_p()
        .ignore_then(if_test)
        .then(if_body)
        .then(kw_else_p().ignore_then(else_body).or_not())
        .map_with(|((test, if_body), opt_else_body), e| {
          Statement::IfElse(IfElse {
            test,
            if_body,
            else_body: opt_else_body.unwrap_or_default(),
          })
        })
        .labelled("if_else")
        .as_context();

      if_else_parser
    };

    let x = choice((
      kw_return_p(),
      call,
      loop_stmt,
      continue_stmt,
      break_stmt,
      load_reg8_const,
      store_a_to_const_addr,
      load_a_from_const_addr,
      if_else,
    ))
    .labelled("statement")
    .as_context();

    x
  })
}
