use super::*;

/// Parses a single item.
pub fn item_p<'src, I, M>(
  make_input: M,
) -> impl Parser<'src, I, FileSpanned<Item>, ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
  M: Fn(&'src [FileSpanned<TokenTree>], FileSpan) -> I + Copy + 'src,
{
  let f = function_p(make_input).map(Item::Function);
  let c = const_p(make_input).map(Item::Const);
  let s = static_p(make_input).map(Item::Static);

  choice((f, c, s))
    .map_with(FileSpanned::from_extras)
    .labelled("item")
    .as_context()
}

/// Parses any keyword which starts a new item.
pub fn item_sep_p<'src, I>(
) -> impl Parser<'src, I, (), ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
{
  select! {
    Lone(KwFn) => (),
    Lone(KwConst) => (),
    Lone(KwStatic) => (),
  }
}
