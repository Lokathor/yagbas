use super::*;

/// Constant definition
///
/// This associates a name to a particular constant expression. A const is only
/// used during compile time, it doesn't add data in the compiled binary.
#[derive(Debug, Clone)]
pub struct AstConst {
  pub file_id: FileID,
  pub name: S<StrID>,
  pub expr: S<Expr>,
}
impl AstConst {
  pub fn expand_size_of_static(
    &mut self, static_sizes: &HashMap<StrID, i32>,
    err_bucket: &mut Vec<YagError>,
  ) {
    self.expr.0.expand_size_of_static(static_sizes, err_bucket);
  }
}

/// Parse one [Const]
pub(crate) fn const_p<'src, I, M>(
  make_input: M,
) -> impl Parser<'src, I, AstConst, AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
  M: Fn(&'src [(TokenTree, SimpleSpan)], SimpleSpan) -> I + Copy + 'src,
{
  let keyword = select! { TokenTree::Lone(Token::KwConst) => () };
  let name = ident_p().map_with(|i, ex| S::from_extras(i, ex));
  let expr = expr_p(make_input); // pre-spanned

  keyword.ignore_then(name).then_ignore(equal_p()).then(expr).map_with(
    |(name, expr), ex| {
      let state: &mut SimpleState<&'static FileData> = ex.state();
      let file_id: FileID = state.id();
      AstConst { file_id, name, expr }
    },
  )
}
