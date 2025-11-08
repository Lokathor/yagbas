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

use chumsky::error::Rich;
use chumsky::prelude::SimpleSpan;
use core::{mem::replace, num::NonZeroUsize};
use derive_more::Display;
use internal_iterator_rec::InternalIterator;
use std::{collections::HashMap, path::PathBuf};
use str_id::StrID;
use core::mem::size_of;

pub mod error;
pub use error::*;

pub mod file_data;
pub use file_data::*;

pub mod token;
pub use token::*;

pub mod token_tree;
pub use token_tree::*;

pub type Span32 = SimpleSpan<u32>;

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub enum Expr {
  #[default]
  ExprError,
  
  NumLit(Box<ExprNumLit>),
  Ident(Box<ExprIdent>),
  List(Box<Vec<ExprUnOp>>),
  Call(Box<ExprCall>),
  Macro(Box<ExprMacro>),
  Struct(Box<ExprStruct>),
  
  Deref(Box<ExprUnOp>),
  Neg(Box<ExprUnOp>),
  Ref(Box<ExprUnOp>),
  
  Add(Box<ExprBinOp>),
  Sub(Box<ExprBinOp>),
  Mul(Box<ExprBinOp>),
  Div(Box<ExprBinOp>),
  Mod(Box<ExprBinOp>),
  ShiftLeft(Box<ExprBinOp>),
  ShiftRight(Box<ExprBinOp>),
  BitAnd(Box<ExprBinOp>),
  BitOr(Box<ExprBinOp>),
  BitXor(Box<ExprBinOp>),
  BoolAnd(Box<ExprBinOp>),
  BoolOr(Box<ExprBinOp>),
  Index(Box<ExprBinOp>),
  Dot(Box<ExprBinOp>),
  Eq(Box<ExprBinOp>),
  Ne(Box<ExprBinOp>),
  Lt(Box<ExprBinOp>),
  Le(Box<ExprBinOp>),
  Gt(Box<ExprBinOp>),
  Ge(Box<ExprBinOp>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExprNumLit {
  lit: StrID,
  lit_span: Span32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExprIdent {
  ident: StrID,
  ident_span: Span32,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExprUnOp {
  inner: Expr,
  inner_span: Span32,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExprBinOp {
  lhs: Expr,
  lhs_span: Span32,
  rhs: Expr,
  rhs_span: Span32,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExprCall {
  target: StrID,
  target_span: Span32,
  args: Vec<ExprUnOp>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExprMacro {
  target: StrID,
  target_span: Span32,
  args: Vec<ExprUnOp>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExprStruct {
  ty: StrID,
  ty_span: Span32,
  args: Vec<FieldAssign>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub enum FieldAssign {
  #[default]
  FieldAssignError,
  Ident(StrID, Span32),
  IdentEq(StrID, Span32, Expr, Span32),
}

#[test]
fn test_expr_size() {
  // note(lokathor): any change in size might be justified (and so we would update this test), but we should still take note of it happening.
  assert_eq!(size_of::<Expr>(), size_of::<[usize;2]>());
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub enum AstAttribute {
  #[default]
  AttributeError,
  /// like `#[hram]`
  Ident(StrID, Span32),
  /// like `#[game_revision = 2]`
  Assignment(StrID, Span32, Expr),
  /// like `#[location($FF00)]`
  Call(StrID, Span32, Vec<AstAttribute>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AstConst {
  pub name: StrID,
  pub name_span: Span32,
  pub ty: StrID,
  pub ty_span: Span32,
  pub expr: Expr,
  pub expr_span: Span32,
  pub attributes: Vec<Expr>,
  pub file_id: FileID,
  pub total_span: Span32,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AstStatic {
  pub name: StrID,
  pub name_span: Span32,
  pub ty: StrID,
  pub ty_span: Span32,
  pub expr: Expr,
  pub expr_span: Span32,
  pub attributes: Vec<Expr>,
  pub file_id: FileID,
  pub total_span: Span32,
  pub mutability: DataMutability,
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub enum DataMutability {
  #[default]
  Immutable,
  Mutable,
  MemoryMappedIO,
}
