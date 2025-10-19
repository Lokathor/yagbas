#![forbid(unsafe_code)]
#![allow(unused_mut)]
#![allow(unused_imports)]
#![allow(unused_parens)]
#![allow(clippy::ptr_arg)]
#![allow(clippy::let_and_return)]
#![allow(clippy::type_complexity)]
#![allow(clippy::double_parens)]
#![allow(clippy::from_str_radix_10)]
#![allow(clippy::diverging_sub_expression)]
#![allow(clippy::needless_lifetimes)]
#![allow(clippy::match_single_binding)]
#![allow(clippy::write_with_newline)]
#![allow(clippy::unused_unit)]
#![allow(clippy::only_used_in_recursion)]
#![allow(unused_braces)]
#![warn(missing_debug_implementations)]
#![warn(missing_copy_implementations)]

//! Yagbas is a compiler for a language of the same name.

use chumsky::error::Rich;
use chumsky::prelude::SimpleSpan;
use core::{mem::replace, num::NonZeroUsize};
use derive_more::Display;
use internal_iterator_rec::InternalIterator;
use std::{collections::HashMap, path::PathBuf};
use str_id::StrID;

pub mod file_data;
pub use file_data::*;

pub mod token;
pub use token::*;

pub mod token_tree;
pub use token_tree::*;

pub mod ast;
pub use ast::*;

pub mod blocks;
pub use blocks::*;

pub mod error;
pub use error::*;
