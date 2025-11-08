use super::*;

/// An expression is a thing that can be made of sub-expressions.
///
/// Within the AST, expressions are untyped. Applying types (and generating type
/// errors) happens after the basic AST is parsed.
#[derive(Debug, Clone, Default)]
pub enum Expr {
  // non-recursive
  /// Any error during expression processing
  #[default]
  ExprError,

  /// A value produced by an intermediate computation during compilation.
  Val(i32),

  /// The memory at a given address.
  MemAddr(u16),

  /// Reference to a static value.
  RefToStatic(StrID),

  /// `123`, `$FF`, `%10_01_00_00`, etc
  NumLit(StrID),

  /// `main`, `memcpy`, etc
  Ident(StrID),

  /// `a`, `hl`, etc
  Reg(Register),

  /// `true` or `false`
  Bool(bool),

  // recurse many
  /// `{ expr0, expr1, ..., exprN }`
  List(Box<Vec<S<Expr>>>),

  /// `fn_name(arg0, arg1, ..., argN)`
  ///
  /// The encoding is that the name (and its span) is put as the last element of
  /// the vec.
  Call(Box<Vec<S<Expr>>>),

  /// `macro_name!( expr0, expr1, ... exprN )`
  ///
  /// The encoding is that the name (and its span) is put as the last element of
  /// the vec.
  MacroUse(Box<Vec<S<Expr>>>),

  /// `LcdCtrl { display_on, bg_on }`, `Position { x= 15, y= 20 }`
  ///
  /// Both bitstructs and standard structs use this form. The encoding is that
  /// the name (and its span) is pushed onto the end of the vec when packing it.
  Structure(Box<Vec<S<Expr>>>),

  // recurse 2
  /// `array.0`, `b.LcdCtrl.bg_on`, etc
  Dot(Box<[S<Self>; 2]>),

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

  // recurse 1
  /// `-12`
  Neg(Box<S<Self>>),

  /// `&12`
  Ref(Box<S<Self>>),

  /// `12++`
  Inc(Box<S<Self>>),

  /// `12--`
  Dec(Box<S<Self>>),

  /// `[hl]`
  Deref(Box<S<Self>>),
}
impl Expr {
  /// Returns the slice of all inner expressions.
  ///
  /// This is mostly used when we want to pass an expression edit "through" a
  /// layer that does not care about the current edit.
  pub fn inner_expr_mut(&mut self) -> &mut [S<Expr>] {
    match self {
      Self::ExprError
      | Self::Val(_)
      | Self::NumLit(_)
      | Self::Ident(_)
      | Self::Bool(_)
      | Self::Reg(_)
      | Self::RefToStatic(_)
      | Self::MemAddr(_) => &mut [],
      Self::Structure(xs) | Self::MacroUse(xs) | Self::Call(xs) => {
        // Note(Lokathor): we want to return the slice of the inner expr values,
        // but not the macro's name (the last element of the vec).
        xs.as_mut_slice().split_last_mut().unwrap().1
      }
      Self::List(xs) => xs.as_mut_slice(),
      Self::Dot(b)
      | Self::Assign(b)
      | Self::Add(b)
      | Self::Sub(b)
      | Self::Mul(b)
      | Self::Div(b)
      | Self::BitAnd(b)
      | Self::BitOr(b)
      | Self::BitXor(b)
      | Self::Eq(b)
      | Self::Ne(b)
      | Self::Lt(b)
      | Self::Le(b)
      | Self::Gt(b)
      | Self::Ge(b)
      | Self::ShiftLeft(b)
      | Self::ShiftRight(b)
      | Self::Mod(b) => b.as_mut(),
      Self::Neg(x)
      | Self::Ref(x)
      | Self::Deref(x)
      | Self::Inc(x)
      | Self::Dec(x) => core::slice::from_mut(&mut *x),
    }
  }
}

macro_rules! infix_maker {
  ($f: path) => {
    |left, _op, right, extras| {
      S::from_extras($f(Box::new([left, right])), extras)
    }
  };
}
macro_rules! prefix_maker {
  ($f: path) => {
    |_op, atom, extras| S::from_extras($f(Box::new(atom)), extras)
  };
}
macro_rules! postfix_maker {
  ($f: path) => {
    |atom, _op, extras| S::from_extras($f(Box::new(atom)), extras)
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
      let call = ident_p()
        .map_with(S::from_extras)
        .then(
          expr
            .clone()
            .separated_by(comma_p())
            .collect::<Vec<_>>()
            .nested_in(parens_content_p(make_input)),
        )
        .map(|(S(name, name_span), mut args)| {
          let name_ex = S(Expr::Ident(name), name_span);
          args.push(name_ex);
          Expr::Call(Box::new(args))
        })
        .labelled("call_expr")
        .as_context();
      let macro_ = ident_p()
        .map_with(S::from_extras)
        .then_ignore(exclamation_p())
        .then(
          expr
            .clone()
            .separated_by(comma_p())
            .collect::<Vec<_>>()
            .nested_in(parens_content_p(make_input)),
        )
        .map(|(S(name, name_span), mut args)| {
          let name_ex = S(Expr::Ident(name), name_span);
          args.push(name_ex);
          Expr::MacroUse(Box::new(args))
        })
        .labelled("macro_expr")
        .as_context();
      let ident =
        ident_p().map(Expr::Ident).labelled("ident_expr").as_context();
      let num = numlit_p().map(Expr::NumLit);
      let register = register_p().map(Expr::Reg);
      let bool = bool_p().map(Expr::Bool);
      let deref = expr
        .clone()
        .nested_in(brackets_content_p(make_input))
        .map(|expr| Expr::Deref(Box::new(expr)))
        .labelled("deref_expr")
        .as_context();
      let list = newline_p()
        .repeated()
        .ignore_then(expr.clone())
        .separated_by(comma_p().padded_by(newline_p().repeated()))
        .allow_trailing()
        .collect::<Vec<_>>()
        .then_ignore(newline_p().repeated())
        .nested_in(braces_content_p(make_input))
        .map(|exprs| Expr::List(Box::new(exprs)))
        .labelled("list_expr")
        .as_context();
      let structure_lit = ident_p()
        .map_with(S::from_extras)
        .then(
          newline_p()
            .repeated()
            .ignore_then(expr.clone())
            .separated_by(comma_p().padded_by(newline_p().repeated()))
            .allow_trailing()
            .collect::<Vec<_>>()
            .then_ignore(newline_p().repeated())
            .nested_in(braces_content_p(make_input)),
        )
        .map(|(S(name, name_span), mut expr)| {
          expr.push(S(Expr::Ident(name), name_span));
          Expr::Structure(Box::new(expr))
        })
        .labelled("structure_expr")
        .as_context();
      let parens = expr
        .clone()
        .nested_in(parens_content_p(make_input))
        .map(|S(expr, _span)| expr);
      // Note(Lokathor): some stuff is "ident and then more", so just an ident
      // has to go at the end of the list.
      choice((
        call,
        macro_,
        structure_lit,
        num,
        register,
        bool,
        deref,
        list,
        parens,
        ident,
      ))
      .map_with(S::from_extras)
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
  .labelled("expr")
  .as_context()
}

impl core::fmt::Display for Expr {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    use core::fmt::{Debug, Display};
    match self {
      Self::NumLit(lit) => Display::fmt(&lit, f),
      Self::Ident(i) => Display::fmt(&i, f),
      Self::Reg(r) => Display::fmt(&r, f),
      Self::Dec(x) => {
        Display::fmt(&x, f)?;
        Display::fmt("--", f)
      }
      Self::Inc(x) => {
        Display::fmt(&x, f)?;
        Display::fmt("++", f)
      }
      Self::Ref(x) => {
        Display::fmt("&", f)?;
        Display::fmt(&x, f)
      }
      Self::MemAddr(u) => {
        Display::fmt("Mem[", f)?;
        write!(f, "${u:04X}")?;
        Display::fmt("]", f)
      }
      Self::Deref(x) => {
        Display::fmt("[", f)?;
        Display::fmt(x, f)?;
        Display::fmt("]", f)
      }
      Self::MacroUse(xs) => {
        let xs_len = xs.len();
        if let Some(name) = xs.last() {
          Display::fmt(&name, f)?;
        }
        Display::fmt("!(", f)?;
        for (i, x) in xs.iter().take(xs_len.saturating_sub(1)).enumerate() {
          if i > 0 {
            Display::fmt(", ", f)?;
          }
          Display::fmt(&x, f)?;
        }
        Display::fmt(")", f)
      }
      Self::Call(xs) => {
        let xs_len = xs.len();
        if let Some(name) = xs.last() {
          Display::fmt(&name, f)?;
        }
        Display::fmt("(", f)?;
        for (i, x) in xs.iter().take(xs_len.saturating_sub(1)).enumerate() {
          if i > 0 {
            Display::fmt(", ", f)?;
          }
          Display::fmt(&x, f)?;
        }
        Display::fmt(")", f)
      }
      Self::Assign(b) => {
        let [lhs, rhs] = b.as_ref();
        Display::fmt(&lhs, f)?;
        Display::fmt(" = ", f)?;
        Display::fmt(&rhs, f)
      }
      Self::Ne(b) => {
        let [lhs, rhs] = b.as_ref();
        Display::fmt(&lhs, f)?;
        Display::fmt(" != ", f)?;
        Display::fmt(&rhs, f)
      }
      Self::Eq(b) => {
        let [lhs, rhs] = b.as_ref();
        Display::fmt(&lhs, f)?;
        Display::fmt(" == ", f)?;
        Display::fmt(&rhs, f)
      }
      _otherwise => Debug::fmt(self, f),
    }
  }
}
