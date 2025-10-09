use crate::{FileData, FileID, Token, TokenTree, YagError, trees_of};
use chumsky::{
  extra::{Err, Full, ParserExtra, SimpleState},
  input::{BorrowInput, MapExtra, ValueInput},
  prelude::*,
};
use derive_more::Display;
use std::{collections::HashMap, ops::Range};
use str_id::StrID;

/// This is a macro instead of a function because I can't figure out what type
/// signature to put on this expression so that Rust lets me actually use the
/// darn thing in more than one place.
macro_rules! statement_recovery_strategy {
  () => {
    via_parser(
      any()
        .and_is(statement_sep_p().not())
        .repeated()
        .at_least(1)
        .to(Statement::StatementError),
    )
  };
}

mod junk_drawer;
use junk_drawer::*; // private junk!

mod s;
pub use s::*;

mod item;
pub use item::*;

mod bitstruct;
pub use bitstruct::*;

mod const_;
pub use const_::*;

mod func;
pub use func::*;

mod static_;
pub use static_::*;

mod struct_;
pub use struct_::*;

mod statement;
pub use statement::*;

mod expr;
pub use expr::*;

mod register;
pub use register::*;

/// The Abstract Syntax Tree for the whole program.
///
/// While each individual item is from a particular source file, the AST
/// combines all the items across all the files. Once it's all collected, we can
/// begin to poke the data to eventually output a compiled program.
#[derive(Debug, Clone, Default)]
pub struct Ast {
  /// All the items in our AST.
  pub items: HashMap<StrID, S<Item>>,

  /// The RAM/ROM sizes (in bytes) of all static items.
  pub static_sizes: HashMap<StrID, i32>,
}
impl Ast {
  /// populate `static_sizes` with values from all the statics.
  pub fn populate_static_sizes(&mut self) {
    self.static_sizes.clear();
    for item in self.items.values() {
      if let Item::Static(AstStatic { name, expr, .. }) = &item.0 {
        if let Expr::List(xs) = &expr.0 {
          self.static_sizes.insert(name.0, xs.len().try_into().unwrap());
        }
      }
    }
  }
}

pub fn parse_num_lit(str_id: StrID) -> Option<i32> {
  let s = str_id.as_str();
  let mut t = 0_i32;
  if let Some(hex_str) = s.strip_prefix('$') {
    // hexadecimal
    for c in hex_str.chars() {
      match c.to_ascii_lowercase() {
        '_' => continue,
        'a'..='f' => {
          t *= 16;
          t += (c as u8 - b'a') as i32;
        }
        '0'..='9' => {
          t *= 16;
          t += (c as u8 - b'0') as i32;
        }
        _ => return None,
      }
    }
  } else if let Some(bin_str) = s.strip_prefix('%') {
    // binary
    for c in bin_str.chars() {
      match c {
        '_' => continue,
        '0'..='1' => {
          t *= 2;
          t += (c as u8 - b'0') as i32;
        }
        _ => return None,
      }
    }
  } else {
    // decimal
    for c in s.chars() {
      match c {
        '_' => continue,
        '0'..='9' => {
          t *= 10;
          t += (c as u8 - b'0') as i32;
        }
        _ => return None,
      }
    }
  }
  Some(t)
}
