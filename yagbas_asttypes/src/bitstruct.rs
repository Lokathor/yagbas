pub use super::*;

/// Bitpacked structured data.
///
/// An 8-bit value, where each bit position can have a field name.
#[derive(Debug, Clone)]
pub struct BitStruct {
  pub file_id: FileID,
  pub name: S<StrID>,
  /// `field_name: bit` list
  pub fields: Vec<(TokenTree, SimpleSpan)>,
}

/// Parse one [BitStruct]
pub(crate) fn bitstruct_p<'src, I>()
-> impl Parser<'src, I, BitStruct, AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  let keyword = select! { TokenTree::Lone(Token::KwBitStruct) => () };
  let name = ident_p().map_with(|i, ex| S::from_extras(i, ex));
  let fields = braces_p();

  keyword.ignore_then(name).then(fields).map_with(|(name, fields), ex| {
    let state: &mut SimpleState<&'static FileData> = ex.state();
    let file_id: FileID = state.id();
    BitStruct { file_id, name, fields }
  })
}
