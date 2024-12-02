use super::*;

/// Parses a single `static` item.
pub fn static_p<'src, I, M>(
  make_input: M,
) -> impl Parser<'src, I, FileSpanned<Static>, ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
  M: Fn(&'src [FileSpanned<TokenTree>], FileSpan) -> I + Copy + 'src,
{
  let name = ident_p().labelled("static_name").as_context();
  let bytes = newline_p()
    .repeated()
    .ignore_then(expression_p(make_input))
    .separated_by(comma_p().padded_by(newline_p().repeated()))
    .collect()
    .then_ignore(comma_p().or_not())
    .then_ignore(newline_p().repeated())
    .nested_in(brackets_content_p(make_input))
    .labelled("static_expression")
    .as_context();

  kw_static_p().ignore_then(name).then_ignore(equal_p()).then(bytes).map_with(
    |(name, bytes), extras| {
      FileSpanned::from_extras(Static { name, bytes }, extras)
    },
  )
}
