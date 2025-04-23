use super::*;

/// Structured data.
#[derive(Debug, Clone)]
pub struct Struct {
  pub file_id: FileID,
  pub name: S<StrID>,
  /// `field_name: field_type` list
  pub fields: Vec<(S<StrID>, S<StrID>)>,
}

/// Parse one [Struct]
pub(crate) fn struct_p<'src, I, M>(
  make_input: M,
) -> impl Parser<'src, I, Struct, AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
  M: Fn(&'src [(TokenTree, SimpleSpan)], SimpleSpan) -> I + Copy + 'src,
{
  let keyword = kw_struct_p();
  let name = ident_p().map_with(S::from_extras);
  let fields = newline_p()
    .repeated()
    .ignore_then(ident_p().map_with(S::from_extras))
    .then_ignore(colon_p())
    .then(ident_p().map_with(S::from_extras))
    .separated_by(comma_p().padded_by(newline_p().repeated()))
    .allow_trailing()
    .collect::<Vec<_>>()
    .nested_in(braces_content_p(make_input));

  keyword.ignore_then(name).then(fields).map_with(|(name, fields), ex| {
    let state: &mut SimpleState<&'static FileData> = ex.state();
    let file_id: FileID = state.id();
    Struct { file_id, name, fields }
  })
}
