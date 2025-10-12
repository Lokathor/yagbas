use super::*;
use internal_iterator_rec::InternalIterator;

/// Function definition
///
/// Defines a body of code that can be executed.
#[derive(Debug, Clone)]
pub struct AstFunc {
  pub file_id: FileID,
  pub name: S<StrID>,
  pub args: Vec<(TokenTree, SimpleSpan)>,
  pub body: Vec<S<Statement>>,
}
impl AstFunc {
  pub fn expand_size_of_static(
    &mut self, static_sizes: &HashMap<StrID, i32>,
    err_bucket: &mut Vec<YagError>,
  ) {
    self.body.iter_mut().for_each(|s| {
      s.0.iter_exprs_mut().for_each(|x| {
        x.expand_size_of_static(static_sizes, err_bucket);
      })
    });
  }

  pub fn iter_statements_mut(
    &mut self,
  ) -> impl Iterator<Item = &mut S<Statement>> + '_ {
    self.body.iter_mut()
  }
}

/// Parse one [Func]
pub(crate) fn func_p<'src, I, M>(
  make_input: M,
) -> impl Parser<'src, I, AstFunc, AstExtras<'src>> + Clone
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
      AstFunc { file_id, name, args, body }
    },
  )
}
