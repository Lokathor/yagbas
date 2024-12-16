#![allow(unused)]
#![allow(clippy::ptr_arg)]
#![allow(clippy::let_and_return)]
#![allow(clippy::type_complexity)]
#![allow(clippy::double_parens)]
#![allow(clippy::from_str_radix_10)]
#![allow(clippy::diverging_sub_expression)]
#![allow(clippy::needless_lifetimes)]

//! Yagbas is a compiler for a language of the same name.
//!
//! * This package is on crates.io so that people can `cargo install yagbas` and
//!   get the compiler that way.
//! * Only the compiler's command line interface and the language that it
//!   compiles are covered by the semantic version number.
//! * The library content of the `yagbas` package (the documentation you're
//!   reading right now) is **NOT** covered by the semantic version of the
//!   package.

use crate::{
  ast::data::{
    Call, Const, Expression, Function, IfElse, Item, Loop, Register, Statement,
    Static, Token, Token::*, TokenTree, TokenTree::*,
  },
  errors::YagError,
  file_span::FileSpan,
  file_spanned::FileSpanned,
  internal_iterator_mut::InternalIteratorMut,
  src_file::{SrcFile, SrcID},
  str_id::StrID,
};
use core::ops::ControlFlow;
use internal_iterator::InternalIterator;

pub mod asm;
pub mod ast;
pub mod errors;
pub mod file_span;
pub mod file_spanned;
pub mod internal_iterator_mut;
pub mod src_file;
pub mod str_id;

pub fn read_src_files(
  paths: &[String], err_bucket: &mut Vec<YagError>,
) -> Vec<SrcFile> {
  paths
    .iter()
    .map(SrcFile::read_from_path)
    .flat_map(|result| match result {
      Err(e) => {
        err_bucket.push(e);
        None
      }
      Ok(t) => Some(t),
    })
    .collect()
}
