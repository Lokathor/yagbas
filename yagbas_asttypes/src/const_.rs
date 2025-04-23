use super::*;

/// Constant definition
///
/// This associates a name to a particular constant expression. A const is only
/// used during compile time, it doesn't add data in the compiled binary.
#[derive(Debug, Clone)]
pub struct Const {
  pub file_id: FileID,
  pub name: S<StrID>,
  pub expr: S<Expr>,
}

/// Parse one [Const]
pub(crate) fn const_p<'src, I, M>(
  make_input: M,
) -> impl Parser<'src, I, Const, AstExtras<'src>> + Clone
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
      Const { file_id, name, expr }
    },
  )
}
