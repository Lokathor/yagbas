#![allow(unused)]

use super::*;

/// An expression is a thing that can be made of sub-expressions.
///
/// Within the AST, expressions are untyped. Applying types (and generating type
/// errors) happens after the basic AST is parsed.
#[derive(Debug, Clone)]
pub enum Expr {
  /// `123`, `$FF`, `%10_01_00_00`, etc
  NumLit(StrID),

  /// `main`, `memcpy`, etc
  Ident(StrID),

  /// `a`, `hl`, etc
  Reg(Register),

  /// `LcdCtrl { display_on, bg_on }`
  BitStructLit(Box<Vec<S<StrID>>>),

  /// `Position { x: 15, y: 20 }`
  StructLit(Box<Vec<(S<StrID>, S<Self>)>>),

  /// `{ expr0, expr1, ..., exprN }`
  List(Box<Vec<S<Expr>>>),

  /// `[hl]`
  Deref(Box<Vec<S<Expr>>>),

  /// `macro_name!( expr0, expr1, ... exprN )`
  MacroUse(Box<Vec<S<Expr>>>),

  /// `array.0`, `b.LcdCtrl.bg_on`, etc
  Dot(Box<[S<Self>; 2]>),

  /// `-12`
  Neg(Box<S<Self>>),

  /// `&12`
  Ref(Box<S<Self>>),

  /// `12++`
  Inc(Box<S<Self>>),

  /// `12--`
  Dec(Box<S<Self>>),

  /// `a = 12`
  Assign(Box<[S<Self>; 2]>),

  /// `12 + 4`
  Add(Box<[S<Self>; 2]>),

  /// `12 - 4`
  Sub(Box<[S<Self>; 2]>),

  /// `12 * 4`
  Mul(Box<[S<Self>; 2]>),

  /// `12 / 4`
  Div(Box<[S<Self>; 2]>),

  /// `12 & 4`
  BitAnd(Box<[S<Self>; 2]>),

  /// `12 | 4`
  BitOr(Box<[S<Self>; 2]>),

  /// `12 ^ 4`
  BitXor(Box<[S<Self>; 2]>),

  /// `12 == 4`
  Eq(Box<[S<Self>; 2]>),

  /// `12 != 4`
  Ne(Box<[S<Self>; 2]>),

  /// `12 < 4`
  Lt(Box<[S<Self>; 2]>),

  /// `12 > 4`
  Gt(Box<[S<Self>; 2]>),

  /// `12 <= 4`
  Le(Box<[S<Self>; 2]>),

  /// `12 >= 4`
  Ge(Box<[S<Self>; 2]>),

  /// `12 << 4`
  ShiftLeft(Box<[S<Self>; 2]>),

  /// `12 >> 4`
  ShiftRight(Box<[S<Self>; 2]>),

  /// `12 % 4`
  Mod(Box<[S<Self>; 2]>),

  /// Any error during expression processing
  ExprError,
}

macro_rules! infix_maker {
  ($f: path) => {
    |left, op, right, extras| {
      S::from_extras($f(Box::new([left, right])), extras)
    }
  };
}
macro_rules! prefix_maker {
  ($f: path) => {
    |op, atom, extras| S::from_extras($f(Box::new(atom)), extras)
  };
}
macro_rules! postfix_maker {
  ($f: path) => {
    |atom, op, extras| S::from_extras($f(Box::new(atom)), extras)
  };
}

/// Parse one [Expr], with spanning pre-applied
pub(crate) fn expr_p<'src, I, M>(
  make_input: M,
) -> impl Parser<'src, I, S<Expr>, AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
  M: Fn(&'src [(TokenTree, SimpleSpan)], SimpleSpan) -> I + Copy + 'src,
{
  use chumsky::pratt::*;

  recursive(|expr| {
    let atom = {
      let ident = ident_p().map(Expr::Ident);
      let num = numlit_p().map(Expr::NumLit);
      let register = register_p().map(Expr::Reg);
      let parens = expr
        .clone()
        .nested_in(parens_content_p(make_input))
        .map(|S(expr, _span)| expr);

      choice((ident, num, register, parens)).map_with(S::from_extras)
    };

    let with_pratt = atom.pratt((
      infix(right(1), equal_p(), infix_maker!(Expr::Assign)),
      infix(left(5), cmp_eq_p(), infix_maker!(Expr::Eq)),
      infix(left(5), cmp_ne_p(), infix_maker!(Expr::Ne)),
      infix(left(5), less_than_p(), infix_maker!(Expr::Lt)),
      infix(left(5), greater_than_p(), infix_maker!(Expr::Gt)),
      infix(left(5), cmp_le_p(), infix_maker!(Expr::Le)),
      infix(left(5), cmp_ge_p(), infix_maker!(Expr::Ge)),
      infix(left(6), pipe_p(), infix_maker!(Expr::BitOr)),
      infix(left(7), caret_p(), infix_maker!(Expr::BitXor)),
      infix(left(8), ampersand_p(), infix_maker!(Expr::BitAnd)),
      infix(left(9), double_lt_p(), infix_maker!(Expr::ShiftLeft)),
      infix(left(9), double_gt_p(), infix_maker!(Expr::ShiftRight)),
      infix(left(10), plus_p(), infix_maker!(Expr::Add)),
      infix(left(10), minus_p(), infix_maker!(Expr::Sub)),
      infix(left(11), asterisk_p(), infix_maker!(Expr::Mul)),
      infix(left(11), slash_p(), infix_maker!(Expr::Div)),
      infix(left(11), percent_p(), infix_maker!(Expr::Mod)),
      prefix(12, minus_p(), prefix_maker!(Expr::Neg)),
      prefix(12, ampersand_p(), prefix_maker!(Expr::Ref)),
      postfix(12, plusplus_p(), postfix_maker!(Expr::Inc)),
      postfix(12, minusminus_p(), postfix_maker!(Expr::Dec)),
      infix(left(15), period_p(), infix_maker!(Expr::Dot)),
    ));

    with_pratt
  })
}
