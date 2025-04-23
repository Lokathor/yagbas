use super::*;

/// Function definition
///
/// Defines a body of code that can be executed.
#[derive(Debug, Clone)]
pub struct Func {
  pub file_id: FileID,
  pub name: S<StrID>,
  pub args: Vec<(TokenTree, SimpleSpan)>,
  pub body: Vec<S<Statement>>,
}

/// Parse one [Func]
pub(crate) fn func_p<'src, I, M>(
  make_input: M,
) -> impl Parser<'src, I, Func, AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
  M: Fn(&'src [(TokenTree, SimpleSpan)], SimpleSpan) -> I + Copy + 'src,
{
  let keyword = select! { TokenTree::Lone(Token::KwFn) => () };
  let name = ident_p().map_with(|i, ex| S::from_extras(i, ex));
  let args = parens_p();
  let body = statement_p(make_input)
    .recover_with(statement_recovery_strategy!())
    .map_with(S::from_extras)
    .separated_by(statement_sep_p().repeated().at_least(1))
    .allow_leading()
    .allow_trailing()
    .collect::<Vec<_>>()
    .nested_in(braces_content_p(make_input));

  keyword.ignore_then(name).then(args).then(body).map_with(
    |((name, args), body), ex| {
      let state: &mut SimpleState<&'static FileData> = ex.state();
      let file_id: FileID = state.id();
      Func { file_id, name, args, body }
    },
  )
}
