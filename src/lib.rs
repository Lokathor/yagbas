#![forbid(unsafe_code)]
#![allow(unused_mut)]
#![allow(unused_imports)]
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

//! Yagbas is a compiler for a language of the same name.

use core::{mem::replace, num::NonZeroUsize};
use derive_more::Display;
use str_id::StrID;
use std::collections::HashMap;

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
