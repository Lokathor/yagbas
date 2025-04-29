use super::*;

/// Static definition
///
/// This is a block of data that's either in ROM or RAM. Generally this doesn't
/// represent code, but instead some other form of data that the program uses,
/// such as tile data.
#[derive(Debug, Clone)]
pub struct AstStatic {
  pub file_id: FileID,
  pub name: S<StrID>,
  pub expr: S<Expr>,
}

/// Parse one [Static]
pub(crate) fn static_p<'src, I, M>(
  make_input: M,
) -> impl Parser<'src, I, AstStatic, AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
  M: Fn(&'src [(TokenTree, SimpleSpan)], SimpleSpan) -> I + Copy + 'src,
{
  let keyword = kw_static_p().labelled("static_keyword").as_context();
  let name = ident_p()
    .map_with(|i, ex| S::from_extras(i, ex))
    .labelled("static_name")
    .as_context();
  let expr = expr_p(make_input).labelled("static_expr").as_context();

  keyword.ignore_then(name).then_ignore(equal_p()).then(expr).map_with(
    |(name, expr), ex| {
      let state: &mut SimpleState<&'static FileData> = ex.state();
      let file_id: FileID = state.id();
      AstStatic { file_id, name, expr }
    },
  )
}
