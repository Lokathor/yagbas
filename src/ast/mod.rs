//! Module for the Abstract Syntax Tree types.

use std::{ffi::OsString, ops::Range};
use str_id::StrId;

pub mod parser;

#[derive(Debug, Clone)]
pub struct AstModule {
  pub filename: OsString,
  pub items: Vec<AstItem>,
}

#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub enum AstItem {
  StaticMmio(AstStaticMmio),
  Constant(AstConstant),
  Function(AstFunction),
  ErrAstItem,
}

#[derive(Debug, Clone)]
pub enum AstValExpr {
  ErrAstValExpr,
  Reference(Box<Self>),
  Dereference(Box<Self>),
}

#[derive(Debug, Clone)]
pub enum AstTypeExpr {
  ErrAstTypeExpr,
  Plain(StrId),
  Array { element_ty: Box<AstTypeExpr>, length: Box<AstValExpr> },
}

#[derive(Debug, Clone)]
pub struct AstStaticMmio {
  pub location: AstValExpr,
  pub location_span: Range<usize>,
  pub name: StrId,
  pub name_span: Range<usize>,
  pub ty: AstTypeExpr,
  pub ty_span: Range<usize>,
}

#[derive(Debug, Clone)]
pub struct AstConstant {
  pub name: StrId,
  pub name_span: Range<usize>,
  pub ty: AstTypeExpr,
  pub ty_span: Range<usize>,
  pub xpr: AstValExpr,
  pub xpr_span: Range<usize>,
}

#[derive(Debug, Clone)]
pub struct AstFunction {
  pub name: StrId,
  pub name_span: Range<usize>,
  pub arguments: Vec<AstFunctionArgument>,
  /// if no return is explicit, it's implicitly still `()`
  pub return_ty: AstTypeExpr,
  /// if no return type is explicit, point to the 0-span position after the
  /// argument list close as being the implicit position.
  pub return_ty_span: Range<usize>,
  pub body: AstBody,
}

#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct AstFunctionArgument {
  pub name: Option<StrId>,
  pub name_span: Range<usize>,
  pub ty: AstTypeExpr,
  pub ty_span: Range<usize>,
}

#[derive(Debug, Clone)]
pub struct AstBody {
  pub statements: Vec<AstStatement>,
}

#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct AstLet {
  pub pattern: AstValExpr,
  pub pattern_span: Range<usize>,
  pub xpr: AstValExpr,
  pub xpr_span: Range<usize>,
}

#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub enum AstStatement {
  Let(AstLet),
  Expression(AstValExpr),
  Item(AstItem),
}
