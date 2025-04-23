use super::*;

/// Static definition
///
/// This is a block of data that's either in ROM or RAM. Generally this doesn't
/// represent code, but instead some other form of data that the program uses,
/// such as tile data.
#[derive(Debug, Clone)]
pub struct Static {
  pub file_id: FileID,
  pub name: S<StrID>,
  pub expr: Vec<(TokenTree, SimpleSpan)>,
}

/// Parse one [Static]
pub(crate) fn static_p<'src, I>()
-> impl Parser<'src, I, Static, AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  let keyword = select! { TokenTree::Lone(Token::KwStatic) => () };
  let name = ident_p().map_with(|i, ex| S::from_extras(i, ex));
  let expr = braces_p();

  keyword.ignore_then(name).then(expr).map_with(|(name, expr), ex| {
    let state: &mut SimpleState<&'static FileData> = ex.state();
    let file_id: FileID = state.id();
    Static { file_id, name, expr }
  })
}
