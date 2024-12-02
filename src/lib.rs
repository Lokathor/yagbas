#![allow(unused)]
#![allow(clippy::ptr_arg)]
#![allow(clippy::let_and_return)]
#![allow(clippy::type_complexity)]
#![allow(clippy::double_parens)]

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
    Call, Expression, Function, Item, Loop, Register, Statement, Token,
    Token::*, TokenTree, TokenTree::*,
  },
  errors::YagError,
  file_span::FileSpan,
  file_spanned::FileSpanned,
  src_file::SrcID,
  str_id::StrID,
};

pub mod ast;
pub mod errors;
pub mod file_span;
pub mod file_spanned;
pub mod src_file;
pub mod str_id;
