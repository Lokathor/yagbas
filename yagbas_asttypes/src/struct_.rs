use super::*;

/// Structured data.
#[derive(Debug, Clone)]
pub struct Struct {
  pub file_id: FileID,
  pub name: S<StrID>,
  /// `field_name: field_type` list
  pub fields: Vec<(TokenTree, SimpleSpan)>,
}

/// Parse one [Struct]
pub(crate) fn struct_p<'src, I>()
-> impl Parser<'src, I, Struct, AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  let keyword = select! { TokenTree::Lone(Token::KwStruct) => () };
  let name = ident_p().map_with(|i, ex| S::from_extras(i, ex));
  let fields = braces_p();

  keyword.ignore_then(name).then(fields).map_with(|(name, fields), ex| {
    let state: &mut SimpleState<&'static FileData> = ex.state();
    let file_id: FileID = state.id();
    Struct { file_id, name, fields }
  })
}
