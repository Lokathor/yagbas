use super::*;

/// Parses a single `const` item.
pub fn const_p<'src, I, M>(
  make_input: M,
) -> impl Parser<'src, I, FileSpanned<Const>, ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
  M: Fn(&'src [FileSpanned<TokenTree>], FileSpan) -> I + Copy + 'src,
{
  let name = ident_p().labelled("const_name").as_context();
  let expression =
    expression_p(make_input).labelled("const_expression").as_context();

  kw_const_p()
    .ignore_then(name)
    .then_ignore(equal_p())
    .then(expression)
    .map_with(|(name, expression), extras| {
      FileSpanned::from_extras(Const { name, expression }, extras)
    })
}
