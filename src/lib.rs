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

pub mod parser;
pub mod tokenizer;
