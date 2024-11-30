use chumsky::input::MapExtra;

use super::*;

/// Parses a single expression, including all inner-expressions.
pub fn expression_p<'src, I, M>(
  make_input: M,
) -> impl Parser<'src, I, FileSpanned<Expression>, ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
  M: Fn(&'src [FileSpanned<TokenTree>], FileSpan) -> I + Copy + 'src,
{
  use chumsky::pratt::*;

  recursive(|expr| {
    let atom = {
      let i = ident_p().map(Expression::Ident);
      let n = num_lit_p().map(Expression::NumLit);
      let r = register_p().map(Expression::Register);
      let p = expr.clone().nested_in(parens_content_p(make_input));

      choice((i, n, r, p))
    };

    let with_pratt = atom.pratt((
      //
      infix(right(1), equal_p(), |left, op, right, extras| {
        FileSpanned::from_extras(
          Expression::Assign(Box::new(left), Box::new(right)),
          extras,
        )
      }),
    ));

    with_pratt
  })
  .map_with(FileSpanned::from_extras)
}
