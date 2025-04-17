#![allow(unused)]
#![allow(clippy::ptr_arg)]
#![allow(clippy::let_and_return)]
#![allow(clippy::type_complexity)]
#![allow(clippy::double_parens)]
#![allow(clippy::from_str_radix_10)]
#![allow(clippy::diverging_sub_expression)]
#![allow(clippy::needless_lifetimes)]
#![allow(clippy::match_single_binding)]
#![allow(clippy::write_with_newline)]

//! Yagbas is a compiler for a language of the same name.
//!
//! * This package is on crates.io so that people can `cargo install yagbas` and
//!   get the compiler that way.
//! * Only the compiler's command line interface and the language that it
//!   compiles are covered by the semantic version number. The library content
//!   of the `yagbas` package (the documentation you're reading right now) is
//!   **NOT** covered by the semantic version of the package.

pub use internal_iterator_rec::*;
pub use str_id::*;
pub use yagbas_asmtypes::*;
pub use yagbas_srcfiletypes::*;

pub mod ast;
