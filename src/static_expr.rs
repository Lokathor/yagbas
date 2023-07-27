use crate::const_expr::ConstExpr;

use super::*;

/// An expression that evaluates to 0 or more bytes of static data.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StaticExpr {
  RawBytesExprList(Vec<(ConstExpr, SimpleSpan)>),
  RawBytesResolved(Vec<u8>, SimpleSpan),
  Error,
}
impl StaticExpr {
  pub fn parser<'a>(
  ) -> impl Parser<'a, TokenTreeSlice<'a>, Self, ErrRichTokenTree<'a>> + Clone {
    let raw_bytes = Self::raw_bytes();
    let recovery = eat_until_parser(semicolon()).to(Self::Error);

    choice((raw_bytes,)).recover_with(via_parser(recovery)).labelled("StaticExpr")
  }

  fn try_expr_resolve(exprs: Vec<(ConstExpr, SimpleSpan)>) -> Self {
    let mut resolved = vec![];
    for (expr, _span) in exprs.iter() {
      match expr {
        ConstExpr::Value(x) => match u8::try_from(*x) {
          Ok(u) => resolved.push(u),
          Err(_) => return Self::RawBytesExprList(exprs),
        },
        _ => return Self::RawBytesExprList(exprs),
      }
    }
    let start = exprs.first().map(|(_, span)| span.start).unwrap_or(0);
    let end = exprs.last().map(|(_, span)| span.end).unwrap_or(0);
    Self::RawBytesResolved(resolved, SimpleSpan::new(start, end))
  }

  fn raw_bytes<'a>(
  ) -> impl Parser<'a, TokenTreeSlice<'a>, Self, ErrRichTokenTree<'a>> + Clone {
    let name = just(Lone(Ident("raw_bytes"))).ignored();
    let bang = bang().ignored();
    let bytes = ConstExpr::parser()
      .map_with_span(id2)
      .separated_by(comma())
      .collect::<Vec<_>>()
      .then_ignore(comma().or_not())
      .nested_in(select_ref! {
        Parens(tokens) = span => {
          let span: SimpleSpan = span;
          tokens.spanned(span)
        },
      });
    name
      .ignore_then(bang)
      .ignore_then(bytes)
      .map(Self::try_expr_resolve)
      .labelled("RawBytes")
  }
}
