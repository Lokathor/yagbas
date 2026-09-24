#![allow(unused_mut)]
#![allow(unused_braces)]
#![allow(unused_parens)]
#![allow(unused_labels)]
#![allow(clippy::needless_return)]
#![allow(clippy::field_reassign_with_default)]
//#![warn(missing_docs)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![deny(unreachable_patterns)]
#![allow(clippy::useless_format)]
#![allow(clippy::ptr_arg)]
#![allow(clippy::match_single_binding)]

//! Yagbas is a compiler for a language of the same name.
//!
//! The SemVer version of this crate does **not** refer to the crate library. It
//! only refers to the language/compiler portion of the project. Contents of the
//! library and its operation may change at any time.

use crate::{path_id::PathId, span::Span};

#[forbid(unsafe_code)]
pub mod global_id;
#[forbid(unsafe_code)]
pub mod kvec;
#[forbid(unsafe_code)]
pub mod non_max_u32;
#[forbid(unsafe_code)]
pub mod non_max_u64;
#[forbid(unsafe_code)]
pub mod path_id;
#[forbid(unsafe_code)]
pub mod span;

#[forbid(unsafe_code)]
pub mod operators;

pub mod tokenizer;

#[forbid(unsafe_code)]
pub mod cst;

#[forbid(unsafe_code)]
pub mod ast;

#[forbid(unsafe_code)]
pub mod ir_nameres_typecheck;

#[derive(Debug, Clone)]
pub struct YagError {
  pub file_origin: PathId,
  pub span: Span,
  pub message: String,
}

make_global_id!(
  /// Globally unique ID value for a particular [Item](crate::ast::Item).
  ItemId
);

make_global_id!(
  /// Globally unique ID value for a particular [ValueExpr](crate::ast::ValueExpr).
  ValueExprId
);

make_global_id!(
  /// Globally unique ID value for a particular
  /// [Type](crate::ir_nameres_typecheck::type_check::Type).
  TypeId
);

make_global_id!(
  /// Globally unique ID value for a particular [Label](crate::ast::Label).
  LabelId
);

make_global_id!(
  /// Globally unique ID value for a particular type inference variable.
  ///
  /// Inference doesn't happen globally, but with it being a global counter
  /// there's less state to track and reset within the resolver.
  InferenceId
);

make_global_id!(
  /// Globally unique ID value for a particular local variable name.
  LocalNameId
);
