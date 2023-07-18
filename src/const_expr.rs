use super::*;

use std::borrow::Cow;

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
impl ConstExpr {
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

      let atom = choice((macro_use, u8_max, u16_max, ident, lit, parens));

      let neg = minus().ignore_then(atom.clone().map_with_span(id2)).map(
        |(rhs, span)| match &rhs {
          ConstExpr::Value(r) => ConstExpr::Value(r.wrapping_neg()),
          _ => ConstExpr::Sub(
            Box::new((ConstExpr::Value(0), SimpleSpan::from(0..0))),
            Box::new((rhs, span)),
          ),
        },
      );
      let pos = plus().ignore_then(atom.clone());
      let not =
        bang().ignore_then(atom.clone().map_with_span(id2)).map(
          |(rhs, span)| match &rhs {
            ConstExpr::Value(r) => ConstExpr::Value(!*r),
            _ => ConstExpr::Sub(
              Box::new((ConstExpr::Value(0), SimpleSpan::from(0..0))),
              Box::new((rhs, span)),
            ),
          },
        );

      let atom_plus_atom = atom
        .clone()
        .map_with_span(id2)
        .then_ignore(plus())
        .then(atom.clone().map_with_span(id2))
        .map(|((lhs, lh_span), (rhs, rh_span))| match (&lhs, &rhs) {
          (ConstExpr::Value(l), ConstExpr::Value(r)) if l.checked_add(*r).is_some() => {
            ConstExpr::Value(*l + *r)
          }
          _ => ConstExpr::Add(Box::new((lhs, lh_span)), Box::new((rhs, rh_span))),
        });
      let atom_minus_atom = atom
        .clone()
        .map_with_span(id2)
        .then_ignore(minus())
        .then(atom.clone().map_with_span(id2))
        .map(|((lhs, lh_span), (rhs, rh_span))| match (&lhs, &rhs) {
          (ConstExpr::Value(l), ConstExpr::Value(r)) if l.checked_sub(*r).is_some() => {
            ConstExpr::Value(*l - *r)
          }
          _ => ConstExpr::Sub(Box::new((lhs, lh_span)), Box::new((rhs, rh_span))),
        });
      let atom_pipe_atom = atom
        .clone()
        .map_with_span(id2)
        .then_ignore(pipe())
        .then(atom.clone().map_with_span(id2))
        .map(|((lhs, lh_span), (rhs, rh_span))| match (&lhs, &rhs) {
          (ConstExpr::Value(l), ConstExpr::Value(r)) => ConstExpr::Value(*l | *r),
          _ => ConstExpr::Or(Box::new((lhs, lh_span)), Box::new((rhs, rh_span))),
        });
      let atom_caret_atom = atom
        .clone()
        .map_with_span(id2)
        .then_ignore(caret())
        .then(atom.clone().map_with_span(id2))
        .map(|((lhs, lh_span), (rhs, rh_span))| match (&lhs, &rhs) {
          (ConstExpr::Value(l), ConstExpr::Value(r)) => ConstExpr::Value(*l ^ *r),
          _ => ConstExpr::Xor(Box::new((lhs, lh_span)), Box::new((rhs, rh_span))),
        });
      let atom_ampersand_atom = atom
        .clone()
        .map_with_span(id2)
        .then_ignore(ampersand())
        .then(atom.clone().map_with_span(id2))
        .map(|((lhs, lh_span), (rhs, rh_span))| match (&lhs, &rhs) {
          (ConstExpr::Value(l), ConstExpr::Value(r)) => ConstExpr::Value(*l & *r),
          _ => ConstExpr::And(Box::new((lhs, lh_span)), Box::new((rhs, rh_span))),
        });

      choice((
        atom_plus_atom,
        atom_minus_atom,
        atom_pipe_atom,
        atom_caret_atom,
        atom_ampersand_atom,
        neg,
        pos,
        not,
        atom,
      ))
    })
  }
}

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
  if let Some(neg) = lit.strip_prefix('-') {
    lit_to_value(neg).map(|i| -i)
  } else if let Some(pos) = lit.strip_prefix('+') {
    lit_to_value(pos)
  } else if let Some(hex) = lit.strip_prefix('$') {
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
