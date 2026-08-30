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

#[derive(Debug, Clone, Default)]
pub struct AstValExpr {
  pub span: Range<usize>,
  pub kind: AstValExprKind,
}

#[derive(Debug, Clone, Default)]
pub enum AstValExprKind {
  #[default]
  ErrAstValExprKind,
  LiteralNumber(StrId),
  Identifier(StrId),
  Reference(Box<AstValExpr>),
  Dereference(Box<AstValExpr>),
  Multiply(Box<AstValExpr>, Box<AstValExpr>),
}

#[derive(Debug, Clone, Default)]
pub struct AstTypeExpr {
  pub span: Range<usize>,
  pub kind: AstTypeExprKind,
}

#[derive(Debug, Clone, Default)]
pub enum AstTypeExprKind {
  #[default]
  ErrAstTypeExprKind,
  Plain(StrId),
  Array {
    element_ty: Box<AstTypeExpr>,
    length: Box<AstValExpr>,
  },
}

#[derive(Debug, Clone, Default)]
pub struct AstStaticMmio {
  pub span: Range<usize>,
  /// location of the static value within the GB
  pub location: AstValExpr,
  pub name: StrId,
  pub name_span: Range<usize>,
  pub ty: AstTypeExpr,
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
