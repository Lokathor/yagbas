use super::*;

/// Parses a single function item.
pub fn function_p<'src, I, M>(
  make_input: M,
) -> impl Parser<'src, I, FileSpanned<Function>, ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
  M: Fn(&'src [FileSpanned<TokenTree>], FileSpan) -> I + Copy + 'src,
{
  let kw = kw_fn_p();
  let name = ident_p().labelled("fn_name").as_context();
  let args = select! {
    Parens(p) = ex => p,
  }
  .labelled("fn_args")
  .as_context();
  let body = statement_p(make_input)
    .recover_with(statement_recovery_strategy!())
    .separated_by(statement_sep_p().repeated().at_least(1))
    .allow_leading()
    .allow_trailing()
    .collect()
    .nested_in(braces_content_p(make_input))
    .labelled("fn_body")
    .as_context();

  kw.ignore_then(name).then(args).then(body).map_with(
    |((name, args), statements), extras| {
      FileSpanned::from_extras(Function { name, args, statements }, extras)
    },
  )
}
