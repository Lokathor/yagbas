use super::*;

use std::borrow::Cow;

/// An expression that should evaluate to a constant value during compilation.
///
/// A const expression can be assigned a name with a [ConstDecl], or
/// more commonly they will appear directly within an instruction.
///
/// * A const expression *never* contains lone `,` or `;`, so either of those
///   can be used as a recovery point when const expression parsing has failed.
/// * The precedence rules are based on the [Expression Precedence][ref-exp]
///   rules as used by Rust. Specifically:
///   * unary plus / neg / not
///   * add / subtract
///   * bit_and
///   * bit_xor
///   * bit_or
///
/// [ref-exp]:
///     https://doc.rust-lang.org/reference/expressions.html#expression-precedence
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConstExpr {
  Value(i32),
  Ident(StaticStr),
  Add(Box<(Self, SimpleSpan)>, Box<(Self, SimpleSpan)>),
  Sub(Box<(Self, SimpleSpan)>, Box<(Self, SimpleSpan)>),
  Or(Box<(Self, SimpleSpan)>, Box<(Self, SimpleSpan)>),
  And(Box<(Self, SimpleSpan)>, Box<(Self, SimpleSpan)>),
  Xor(Box<(Self, SimpleSpan)>, Box<(Self, SimpleSpan)>),
  Not(Box<(Self, SimpleSpan)>),
  MacroUse { name: (StaticStr, SimpleSpan), args: Vec<(TokenTree, SimpleSpan)> },
  UnknownError(CowStr),
}
// Note(Zesterer): https://gist.github.com/zesterer/e0a896ef16fdc95a4749851ebb0d8461
impl ConstExpr {
  pub const BAD_PARSE: Self = ConstExpr::UnknownError(CowStr::Borrowed("Bad Parse"));

  /// Parses a const expression.
  ///
  /// * Parsing will immediately evaluate operations when possible (including
  ///   that `i32` overflow must not occur), and will otherwise leave evaluation
  ///   work for later.
  pub fn parser<'a>(
  ) -> impl Parser<'a, TokenTreeSlice<'a>, Self, ErrRichTokenTree<'a>> + Clone {
    recursive(|expr| {
      // TODO: this can probably be much more precise later on, probably we
      // should expand it out into being its own full parser really.
      let macro_use = ident()
        .map_with_span(id2)
        .then_ignore(bang())
        .then(select! {Parens(p) => p})
        .map(|(name, args)| Self::MacroUse { name, args });

      // Just for fun we'll parse these two "magical" sequences, even though
      // other `path::ident` stuff won't be available until later.
      let u8_max = just(Lone(Ident("u8")))
        .then(colon())
        .then(colon())
        .then(just(Lone(Ident("MAX"))))
        .to(ConstExpr::Value(u8::MAX as i32));
      let u16_max = just(Lone(Ident("u16")))
        .then(colon())
        .then(colon())
        .then(just(Lone(Ident("MAX"))))
        .to(ConstExpr::Value(u16::MAX as i32));
      let i8_max = just(Lone(Ident("i8")))
        .then(colon())
        .then(colon())
        .then(just(Lone(Ident("MAX"))))
        .to(ConstExpr::Value(i8::MAX as i32));
      let i16_max = just(Lone(Ident("i16")))
        .then(colon())
        .then(colon())
        .then(just(Lone(Ident("MAX"))))
        .to(ConstExpr::Value(i16::MAX as i32));
      let i8_min = just(Lone(Ident("i8")))
        .then(colon())
        .then(colon())
        .then(just(Lone(Ident("MIN"))))
        .to(ConstExpr::Value(i8::MIN as i32));
      let i16_min = just(Lone(Ident("i16")))
        .then(colon())
        .then(colon())
        .then(just(Lone(Ident("MIN"))))
        .to(ConstExpr::Value(i16::MIN as i32));
      let magic_constant = choice((u8_max, u16_max, i8_max, i16_max, i8_min, i16_min));

      let ident = ident().map(Self::Ident);

      let lit = num_lit().map(|lit| match lit_to_value(lit) {
        Ok(i) => Self::Value(i),
        Err(e) => Self::UnknownError(e),
      });

      let parens = expr.clone().nested_in(select_ref! {
        Parens(tokens) = span => {
          let span: SimpleSpan = span;
          tokens.spanned(span)
        },
      });

      let atom = choice((macro_use, magic_constant, ident, lit, parens));

      let unary = choice((minus(), plus(), bang()))
        .map_with_span(id2)
        .repeated()
        .foldr(atom.clone().map_with_span(id2), |(ch, op_span), (rhs, rhs_span)| {
          let span = SimpleSpan::new(op_span.start, rhs_span.end);
          match (ch, &rhs) {
            // `+atom` is accepted and always evaluates to the `atom`, even when
            // we don't know the atom's value right now.
            ('+', _) => (rhs, span),
            // negate or not if we know the value right now, otherwise make an
            // expression node for later evaluation
            ('-', ConstExpr::Value(r)) => (ConstExpr::Value(r.wrapping_neg()), span),
            ('-', _) => (
              ConstExpr::Sub(
                Box::new((ConstExpr::Value(0), SimpleSpan::from(0..0))),
                Box::new((rhs, span)),
              ),
              span,
            ),
            ('!', ConstExpr::Value(r)) => (ConstExpr::Value(!*r), span),
            ('!', _) => (ConstExpr::Not(Box::new((rhs, span))), span),
            _ => unimplemented!(),
          }
        })
        .map(|(expr, _span)| expr)
        .boxed();

      let add_sub = unary
        .clone()
        .map_with_span(id2)
        .foldl(
          choice((plus(), minus()))
            .map_with_span(id2)
            .then(unary.clone().map_with_span(id2))
            .repeated(),
          |(lhs, lhs_span), ((op, _op_span), (rhs, rhs_span))| {
            let span = SimpleSpan::new(lhs_span.start, rhs_span.end);
            match (&lhs, op, &rhs) {
              (ConstExpr::Value(l), '+', ConstExpr::Value(r))
                if l.checked_add(*r).is_some() =>
              {
                (ConstExpr::Value(*l + *r), span)
              }
              (_, '+', _) => (
                ConstExpr::Add(Box::new((lhs, lhs_span)), Box::new((rhs, rhs_span))),
                span,
              ),
              (ConstExpr::Value(l), '-', ConstExpr::Value(r))
                if l.checked_sub(*r).is_some() =>
              {
                (ConstExpr::Value(*l - *r), span)
              }
              (_, '-', _) => (
                ConstExpr::Sub(Box::new((lhs, lhs_span)), Box::new((rhs, rhs_span))),
                span,
              ),
              _ => unimplemented!(),
            }
          },
        )
        .map(|(expr, _span)| expr);

      let bitand = add_sub
        .clone()
        .map_with_span(id2)
        .foldl(
          ampersand()
            .map_with_span(id2)
            .then(add_sub.clone().map_with_span(id2))
            .repeated(),
          |(lhs, lhs_span), ((op, _op_span), (rhs, rhs_span))| {
            let span = SimpleSpan::new(lhs_span.start, rhs_span.end);
            match (&lhs, op, &rhs) {
              (ConstExpr::Value(l), '&', ConstExpr::Value(r)) => {
                (ConstExpr::Value(*l & *r), span)
              }
              (_, '&', _) => (
                ConstExpr::And(Box::new((lhs, lhs_span)), Box::new((rhs, rhs_span))),
                span,
              ),
              _ => unimplemented!(),
            }
          },
        )
        .map(|(expr, _span)| expr);

      let bitxor = bitand
        .clone()
        .map_with_span(id2)
        .foldl(
          caret().map_with_span(id2).then(bitand.clone().map_with_span(id2)).repeated(),
          |(lhs, lhs_span), ((op, _op_span), (rhs, rhs_span))| {
            let span = SimpleSpan::new(lhs_span.start, rhs_span.end);
            match (&lhs, op, &rhs) {
              (ConstExpr::Value(l), '^', ConstExpr::Value(r)) => {
                (ConstExpr::Value(*l ^ *r), span)
              }
              (_, '^', _) => (
                ConstExpr::And(Box::new((lhs, lhs_span)), Box::new((rhs, rhs_span))),
                span,
              ),
              _ => unimplemented!(),
            }
          },
        )
        .map(|(expr, _span)| expr);

      let bitor = bitxor
        .clone()
        .map_with_span(id2)
        .foldl(
          pipe().map_with_span(id2).then(bitxor.clone().map_with_span(id2)).repeated(),
          |(lhs, lhs_span), ((op, _op_span), (rhs, rhs_span))| {
            let span = SimpleSpan::new(lhs_span.start, rhs_span.end);
            match (&lhs, op, &rhs) {
              (ConstExpr::Value(l), '|', ConstExpr::Value(r)) => {
                (ConstExpr::Value(*l | *r), span)
              }
              (_, '|', _) => (
                ConstExpr::And(Box::new((lhs, lhs_span)), Box::new((rhs, rhs_span))),
                span,
              ),
              _ => unimplemented!(),
            }
          },
        )
        .map(|(expr, _span)| expr)
        .boxed();

      bitor
    })
  }
}

/// Parse a numeric literal info a value we can work with.
///
/// * Fails if `i32` overflow would occur.
/// * Fails if a digit is out of range for the number literal type based on the
///   literal's prefix (binary, decimal, or hexadecimal).
pub fn lit_to_value(lit: &str) -> Result<i32, CowStr> {
  fn hex_to_value(hex: &str) -> Result<i32, CowStr> {
    let mut total = 0_i32;
    for ch in hex.chars().filter(|ch| *ch != '_') {
      total = total.checked_mul(16).ok_or(Cow::Borrowed("Overflow"))?;
      let value = match ch as u8 {
        b'a'..=b'f' => i32::from(ch as u8) - i32::from(b'a') + 10,
        b'A'..=b'F' => i32::from(ch as u8) - i32::from(b'A') + 10,
        b'0'..=b'9' => i32::from(ch as u8) - i32::from(b'0'),
        _ => return Err(Cow::Owned(format!("Illegal Hexadecimal Digit: `{ch}`"))),
      };
      total = total.checked_add(value).ok_or(Cow::Borrowed("Overflow"))?;
    }
    Ok(total)
  }
  fn bin_to_value(bin: &str) -> Result<i32, CowStr> {
    let mut total = 0_i32;
    for ch in bin.chars().filter(|ch| *ch != '_') {
      total = total.checked_mul(2).ok_or(Cow::Borrowed("Overflow"))?;
      let value = match ch as u8 {
        b'0'..=b'1' => i32::from(ch as u8) - i32::from(b'0'),
        _ => return Err(Cow::Owned(format!("Illegal Binary Digit: `{ch}`"))),
      };
      total = total.checked_add(value).ok_or(Cow::Borrowed("Overflow"))?;
    }
    Ok(total)
  }
  fn dec_to_value(dec: &str) -> Result<i32, CowStr> {
    let mut total = 0_i32;
    for ch in dec.chars().filter(|ch| *ch != '_') {
      total = total.checked_mul(10).ok_or(Cow::Borrowed("Overflow"))?;
      let value = match ch as u8 {
        b'0'..=b'9' => i32::from(ch as u8) - i32::from(b'0'),
        _ => return Err(Cow::Owned(format!("Illegal Decimal Digit: `{ch}`"))),
      };
      total = total.checked_add(value).ok_or(Cow::Borrowed("Overflow"))?;
    }
    Ok(total)
  }
  //
  if let Some(hex) = lit.strip_prefix('$') {
    hex_to_value(hex)
  } else if let Some(hex) = lit.strip_prefix("0x") {
    hex_to_value(hex)
  } else if let Some(bin) = lit.strip_prefix('%') {
    bin_to_value(bin)
  } else if let Some(bin) = lit.strip_prefix("0b") {
    bin_to_value(bin)
  } else {
    dec_to_value(lit)
  }
}
