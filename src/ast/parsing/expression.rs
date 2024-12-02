use chumsky::input::MapExtra;

use super::*;

macro_rules! infix_maker {
  ($f: path) => {
    |left, op, right, extras| {
      FileSpanned::from_extras($f(Box::new(left), Box::new(right)), extras)
    }
  };
}
macro_rules! prefix_maker {
  ($f: path) => {
    |op, atom, extras| FileSpanned::from_extras($f(Box::new(atom)), extras)
  };
}
macro_rules! postfix_maker {
  ($f: path) => {
    |atom, op, extras| FileSpanned::from_extras($f(Box::new(atom)), extras)
  };
}

/// Parses a single expression, including all inner expressions.
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
      let ident = ident_p().map(Expression::Ident);
      let num = num_lit_p().map(Expression::NumLit);
      let reg = register_p().map(Expression::Register);
      let bool = bool_p().map(Expression::Bool);
      let macro_ = ident_p()
        .then_ignore(exclamation_p())
        .then(select! {
          Parens(p) = ex => p,
        })
        .map(|(name, args)| Expression::Macro(name, args));
      let deref = expr
        .clone()
        .nested_in(brackets_content_p(make_input))
        .map(|fs_expr| Expression::Deref(Box::new(fs_expr)));
      let parens = expr
        .clone()
        .nested_in(parens_content_p(make_input))
        .map(|fs_expr| fs_expr._payload);

      // Note(Lokathor): `macro_` must come before `ident` in the choice list.
      // They both parse an identifier, but `macro_` also parses more after, so
      // we must put the longest parser first to make sure that it can be
      // checked before parsing just the one identifier.
      choice((macro_, ident, num, reg, bool, deref, parens))
        .map_with(FileSpanned::from_extras)
    };

    let with_pratt = atom.pratt((
      infix(right(1), equal_p(), infix_maker!(Expression::Assign)),
      infix(left(5), cmp_eq_p(), infix_maker!(Expression::Eq)),
      infix(left(5), cmp_ne_p(), infix_maker!(Expression::Ne)),
      infix(left(5), less_than_p(), infix_maker!(Expression::Lt)),
      infix(left(5), greater_than_p(), infix_maker!(Expression::Gt)),
      infix(left(5), cmp_le_p(), infix_maker!(Expression::Le)),
      infix(left(5), cmp_ge_p(), infix_maker!(Expression::Ge)),
      infix(left(6), pipe_p(), infix_maker!(Expression::BitOr)),
      infix(left(7), caret_p(), infix_maker!(Expression::BitXor)),
      infix(left(8), ampersand_p(), infix_maker!(Expression::BitAnd)),
      infix(left(9), double_lt_p(), infix_maker!(Expression::ShiftLeft)),
      infix(left(9), double_gt_p(), infix_maker!(Expression::ShiftRight)),
      infix(left(10), plus_p(), infix_maker!(Expression::Add)),
      infix(left(10), minus_p(), infix_maker!(Expression::Sub)),
      infix(left(11), asterisk_p(), infix_maker!(Expression::Mul)),
      infix(left(11), slash_p(), infix_maker!(Expression::Div)),
      infix(left(11), percent_p(), infix_maker!(Expression::Mod)),
      prefix(12, minus_p(), prefix_maker!(Expression::Neg)),
      prefix(12, ampersand_p(), prefix_maker!(Expression::Ref)),
      postfix(12, plusplus_p(), postfix_maker!(Expression::Inc)),
      postfix(12, minusminus_p(), postfix_maker!(Expression::Dec)),
      infix(left(15), period_p(), infix_maker!(Expression::Dot)),
    ));

    with_pratt
  })
  .labelled("expression")
  .as_context()
}
