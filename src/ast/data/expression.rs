use super::*;

#[derive(Debug, Clone, Default, PartialEq)]
pub enum Expression {
  NumLit(FileSpanned<StrID>),
  Ident(FileSpanned<StrID>),
  Register(FileSpanned<Register>),
  Bool(FileSpanned<bool>),
  Macro(FileSpanned<StrID>, FileSpanned<Vec<FileSpanned<TokenTree>>>),

  I32(FileSpanned<i32>),
  StaticIdent(FileSpanned<StrID>),
  RefToStatic(FileSpanned<StrID>),

  /// `[x]`
  Deref(Box<FileSpanned<Self>>),

  /// `x.y`
  Dot(Box<FileSpanned<Self>>, Box<FileSpanned<Self>>),

  /// `-x`
  Neg(Box<FileSpanned<Self>>),
  /// `&x`
  Ref(Box<FileSpanned<Self>>),
  /// `x++`
  Inc(Box<FileSpanned<Self>>),
  /// `x--`
  Dec(Box<FileSpanned<Self>>),

  /// `x * y`
  Mul(Box<FileSpanned<Self>>, Box<FileSpanned<Self>>),
  /// `x / y`
  Div(Box<FileSpanned<Self>>, Box<FileSpanned<Self>>),
  /// `x % y`
  Mod(Box<FileSpanned<Self>>, Box<FileSpanned<Self>>),

  /// `x + y`
  Add(Box<FileSpanned<Self>>, Box<FileSpanned<Self>>),
  /// `x - y`
  Sub(Box<FileSpanned<Self>>, Box<FileSpanned<Self>>),

  /// `x << y`
  ShiftLeft(Box<FileSpanned<Self>>, Box<FileSpanned<Self>>),
  /// `x >> y`
  ShiftRight(Box<FileSpanned<Self>>, Box<FileSpanned<Self>>),

  /// `x & y`
  BitAnd(Box<FileSpanned<Self>>, Box<FileSpanned<Self>>),

  /// `x ^ y`
  BitXor(Box<FileSpanned<Self>>, Box<FileSpanned<Self>>),

  /// `x | y`
  BitOr(Box<FileSpanned<Self>>, Box<FileSpanned<Self>>),

  /// `x == y`
  Eq(Box<FileSpanned<Self>>, Box<FileSpanned<Self>>),
  /// `x != y`
  Ne(Box<FileSpanned<Self>>, Box<FileSpanned<Self>>),
  /// `x < y`
  Lt(Box<FileSpanned<Self>>, Box<FileSpanned<Self>>),
  /// `x > y`
  Gt(Box<FileSpanned<Self>>, Box<FileSpanned<Self>>),
  /// `x <= y`
  Le(Box<FileSpanned<Self>>, Box<FileSpanned<Self>>),
  /// `x >= y`
  Ge(Box<FileSpanned<Self>>, Box<FileSpanned<Self>>),

  /// `x = y`
  Assign(Box<FileSpanned<Self>>, Box<FileSpanned<Self>>),

  #[default]
  ExpressionError,
}
impl Expression {
  /// Maps the closure over any `Macro` atoms within the expression.
  pub fn map_macros<F>(&mut self, op: &mut F)
  where
    F: FnMut(
      FileSpanned<StrID>,
      FileSpanned<Vec<FileSpanned<TokenTree>>>,
    ) -> Expression,
  {
    use Expression::*;
    match self {
      Macro(name, args) => {
        let take_name = FileSpanned::take(name);
        let take_args = FileSpanned::take(args);
        *self = op(take_name, take_args)
      }
      NumLit(_) | Ident(_) | Register(_) | Bool(_) | I32(_)
      | StaticIdent(_) | RefToStatic(_) | ExpressionError => (),
      Deref(file_spanned) | Neg(file_spanned) | Ref(file_spanned)
      | Inc(file_spanned) | Dec(file_spanned) => file_spanned.map_macros(op),
      Dot(file_spanned, file_spanned1)
      | Mul(file_spanned, file_spanned1)
      | Div(file_spanned, file_spanned1)
      | Mod(file_spanned, file_spanned1)
      | Add(file_spanned, file_spanned1)
      | Sub(file_spanned, file_spanned1)
      | ShiftLeft(file_spanned, file_spanned1)
      | ShiftRight(file_spanned, file_spanned1)
      | BitAnd(file_spanned, file_spanned1)
      | BitXor(file_spanned, file_spanned1)
      | BitOr(file_spanned, file_spanned1)
      | Eq(file_spanned, file_spanned1)
      | Ne(file_spanned, file_spanned1)
      | Lt(file_spanned, file_spanned1)
      | Gt(file_spanned, file_spanned1)
      | Le(file_spanned, file_spanned1)
      | Ge(file_spanned, file_spanned1)
      | Assign(file_spanned, file_spanned1) => {
        file_spanned.map_macros(op);
        file_spanned1.map_macros(op);
      }
      RefToStatic(file_spanned) => todo!(),
    }
  }

  /// Maps the closure over any `Macro` atoms within the expression.
  pub fn map_num_lit<F>(&mut self, op: &mut F)
  where
    F: FnMut(FileSpanned<StrID>) -> Expression,
  {
    use Expression::*;
    match self {
      NumLit(num) => {
        let take_num = FileSpanned::take(num);
        *self = op(take_num);
      }
      Macro(_, _) => (),
      Ident(_) | Register(_) | Bool(_) | I32(_) | StaticIdent(_)
      | RefToStatic(_) | ExpressionError => (),
      Deref(file_spanned) | Neg(file_spanned) | Ref(file_spanned)
      | Inc(file_spanned) | Dec(file_spanned) => file_spanned.map_num_lit(op),
      Dot(file_spanned, file_spanned1)
      | Mul(file_spanned, file_spanned1)
      | Div(file_spanned, file_spanned1)
      | Mod(file_spanned, file_spanned1)
      | Add(file_spanned, file_spanned1)
      | Sub(file_spanned, file_spanned1)
      | ShiftLeft(file_spanned, file_spanned1)
      | ShiftRight(file_spanned, file_spanned1)
      | BitAnd(file_spanned, file_spanned1)
      | BitXor(file_spanned, file_spanned1)
      | BitOr(file_spanned, file_spanned1)
      | Eq(file_spanned, file_spanned1)
      | Ne(file_spanned, file_spanned1)
      | Lt(file_spanned, file_spanned1)
      | Gt(file_spanned, file_spanned1)
      | Le(file_spanned, file_spanned1)
      | Ge(file_spanned, file_spanned1)
      | Assign(file_spanned, file_spanned1) => {
        file_spanned.map_num_lit(op);
        file_spanned1.map_num_lit(op);
      }
    }
  }

  /// Maps the closure over any `Ident` atoms within the expression.
  pub fn map_ident<F>(&mut self, op: &mut F)
  where
    F: FnMut(FileSpanned<StrID>) -> Expression,
  {
    use Expression::*;
    match self {
      Ident(i) => {
        let take_ident = FileSpanned::take(i);
        *self = op(take_ident);
      }
      Macro(_, _) => (),
      NumLit(_) | Register(_) | Bool(_) | I32(_) | StaticIdent(_)
      | RefToStatic(_) | ExpressionError => (),
      Deref(file_spanned) | Neg(file_spanned) | Ref(file_spanned)
      | Inc(file_spanned) | Dec(file_spanned) => file_spanned.map_ident(op),
      Dot(file_spanned, file_spanned1)
      | Mul(file_spanned, file_spanned1)
      | Div(file_spanned, file_spanned1)
      | Mod(file_spanned, file_spanned1)
      | Add(file_spanned, file_spanned1)
      | Sub(file_spanned, file_spanned1)
      | ShiftLeft(file_spanned, file_spanned1)
      | ShiftRight(file_spanned, file_spanned1)
      | BitAnd(file_spanned, file_spanned1)
      | BitXor(file_spanned, file_spanned1)
      | BitOr(file_spanned, file_spanned1)
      | Eq(file_spanned, file_spanned1)
      | Ne(file_spanned, file_spanned1)
      | Lt(file_spanned, file_spanned1)
      | Gt(file_spanned, file_spanned1)
      | Le(file_spanned, file_spanned1)
      | Ge(file_spanned, file_spanned1)
      | Assign(file_spanned, file_spanned1) => {
        file_spanned.map_ident(op);
        file_spanned1.map_ident(op);
      }
    }
  }

  /// Maps the closure over any `Ref` atoms within the expression.
  pub fn map_ref<F>(&mut self, op: &mut F)
  where
    F: FnMut(FileSpanned<Expression>) -> Expression,
  {
    use Expression::*;
    match self {
      Ref(i) => {
        let take_ident = FileSpanned::take(i);
        *self = op(take_ident);
      }
      Macro(_, _) => (),
      NumLit(_) | Register(_) | Bool(_) | I32(_) | StaticIdent(_)
      | Ident(_) | RefToStatic(_) | ExpressionError => (),
      Deref(file_spanned) | Neg(file_spanned) | Inc(file_spanned)
      | Dec(file_spanned) => file_spanned.map_ref(op),
      Dot(file_spanned, file_spanned1)
      | Mul(file_spanned, file_spanned1)
      | Div(file_spanned, file_spanned1)
      | Mod(file_spanned, file_spanned1)
      | Add(file_spanned, file_spanned1)
      | Sub(file_spanned, file_spanned1)
      | ShiftLeft(file_spanned, file_spanned1)
      | ShiftRight(file_spanned, file_spanned1)
      | BitAnd(file_spanned, file_spanned1)
      | BitXor(file_spanned, file_spanned1)
      | BitOr(file_spanned, file_spanned1)
      | Eq(file_spanned, file_spanned1)
      | Ne(file_spanned, file_spanned1)
      | Lt(file_spanned, file_spanned1)
      | Gt(file_spanned, file_spanned1)
      | Le(file_spanned, file_spanned1)
      | Ge(file_spanned, file_spanned1)
      | Assign(file_spanned, file_spanned1) => {
        file_spanned.map_ref(op);
        file_spanned1.map_ref(op);
      }
    }
  }

  /// Perform compile time expression simplification.
  ///
  /// This is where we combine const values according to various operations,
  /// like turning `2 + 3` into `5` at compile time.
  pub fn simplify_value(&mut self) {
    // Note(Lokathor): As in normal math, you must first simplify any
    // sub-expressions before simplifying the current expression being examined.
    use Expression::*;
    match self {
      Register(_) | I32(_) | RefToStatic(_) => (),
      Assign(left, right) => {
        left.simplify_value();
        right.simplify_value();
      }
      Deref(x) | Dec(x) | Inc(x) => x.simplify_value(),
      Eq(left, right) | Ne(left, right) => {
        left.simplify_value();
        right.simplify_value();
      }
      BitOr(left, right) => {
        left.simplify_value();
        right.simplify_value();
        match (&mut left._payload, &mut right._payload) {
          (I32(l), I32(r)) => {
            *self = I32(FileSpanned::new(
              l._payload | r._payload,
              left._span.join(right._span),
            ))
          }
          (Bool(l), Bool(r)) => {
            *self = Bool(FileSpanned::new(
              l._payload | r._payload,
              left._span.join(right._span),
            ))
          }
          _ => (),
        }
      }
      other => todo!("{other:?}"),
    }
  }
}
