#![forbid(unsafe_code)]
#![allow(unused_mut)]
#![allow(unused_braces)]
#![allow(unused_parens)]
#![allow(unused_imports)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![allow(clippy::ptr_arg)]
#![allow(clippy::unused_unit)]
#![allow(clippy::double_parens)]
#![allow(clippy::let_and_return)]
#![allow(clippy::type_complexity)]
#![allow(clippy::from_str_radix_10)]
#![allow(clippy::needless_lifetimes)]
#![allow(clippy::write_with_newline)]
#![allow(clippy::match_single_binding)]
#![allow(clippy::only_used_in_recursion)]
#![allow(clippy::diverging_sub_expression)]

//! Yagbas is a compiler for a language of the same name.

use chumsky::{
  error::Rich,
  extra::{Full, SimpleState},
  input::{BorrowInput, Input, ValueInput},
  prelude::*,
};
use core::{
  mem::{replace, size_of},
  num::NonZeroUsize,
  ops::Range,
};
use derive_more::Display;
use internal_iterator_rec::InternalIterator;
use std::{collections::HashMap, path::PathBuf};
use str_id::StrID;

pub mod error;
pub use error::*;

pub mod file_data;
pub use file_data::*;

pub mod token;
pub use token::*;

pub mod token_tree;
pub use token_tree::*;

pub mod parser;
pub use parser::*;

pub mod ast;
pub use ast::*;

pub mod expr;
pub use expr::*;

pub mod statement;
pub use statement::*;

pub type Span32 = SimpleSpan<u32>;
