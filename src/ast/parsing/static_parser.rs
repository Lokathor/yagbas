use crate::ast::{Static, StaticExpr, StaticType};

use super::*;

/// Parses [TokenTree] into specifically a [Static]
pub fn static_p<'src, I, M>(
  make_input: M,
) -> impl Parser<'src, I, Static, ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
  M: Fn(&'src [FileSpanned<TokenTree>], FileSpan) -> I + Copy + 'src,
{
  let name = ident_p()
    .map_with(|i, e| FileSpanned::new(i, e.span()))
    .labelled("static_name")
    .as_context();
  let static_ty = kw_u8_p()
    .ignore_then(semicolon_p())
    .ignore_then(select! {Lone(Ident(i)) if i.as_ref() == "_" => ()})
    .nested_in(nested_bracket_content_p(make_input))
    .map_with(|_, e| FileSpanned::new(StaticType::ArrayInferred, e.span()))
    .labelled("static_type")
    .as_context();
  let static_exp = const_expr_p(make_input)
    .separated_by(comma_p().padded_by(newline_p().repeated()))
    .allow_leading()
    .allow_trailing()
    .collect()
    .nested_in(nested_bracket_content_p(make_input))
    .map_with(|v, e| FileSpanned::new(StaticExpr::Array(v), e.span()))
    .labelled("static_expression")
    .as_context();

  // https://github.com/rust-lang/rust-analyzer/issues/18542
  let x = Parser::map(
    kw_static_p()
      .ignore_then(name)
      .then_ignore(colon_p())
      .then(static_ty)
      .then_ignore(equal_p())
      .then(static_exp),
    |((name, ty), expr)| Static { name, ty, expr },
  )
  .labelled("static")
  .as_context();

  x
}
