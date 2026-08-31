//! Module for the Abstract Syntax Tree types.

use std::ops::Range;
use str_id::{PathId, StrId};

pub mod parser;

#[derive(Debug, Clone)]
pub struct AstModule {
  pub path_id: PathId,
  pub items: Vec<AstItem>,
}

#[derive(Debug, Clone, Default)]
pub struct AstItem {
  pub span: Range<usize>,
  pub kind: AstItemKind,
}

#[derive(Debug, Clone, Default)]
pub enum AstItemKind {
  #[default]
  ErrAstItemKind,
  StaticMmio(AstStaticMmio),
  Constant(AstConstant),
  Function(AstFunction),
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
  Assign(Box<AstValExpr>, Box<AstValExpr>),
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
  pub address: AstValExpr,
  pub name: StrId,
  pub name_span: Range<usize>,
  pub ty: AstTypeExpr,
}

#[derive(Debug, Clone, Default)]
pub struct AstConstant {
  pub name: StrId,
  pub name_span: Range<usize>,
  pub ty: AstTypeExpr,
  pub xpr: AstValExpr,
}

#[derive(Debug, Clone, Default)]
pub struct AstFunction {
  pub name: StrId,
  pub name_span: Range<usize>,
  pub arguments: Vec<AstFunctionArgument>,
  /// if no return is explicit, it's implicitly still `()`
  pub return_ty: AstTypeExpr,
  pub body: AstBody,
}

#[derive(Debug, Clone)]
pub struct AstFunctionArgument {
  pub name: Option<StrId>,
  pub name_span: Range<usize>,
  pub ty: AstTypeExpr,
  pub ty_span: Range<usize>,
}

#[derive(Debug, Clone, Default)]
pub struct AstBody {
  pub statements: Vec<AstStatement>,
}

#[derive(Debug, Clone)]
pub struct AstLet {
  pub pattern: AstValExpr,
  pub xpr: AstValExpr,
}

#[derive(Debug, Clone, Default)]
pub struct AstStatement {
  pub span: Range<usize>,
  pub kind: AstStatementKind,
}

#[derive(Debug, Clone, Default)]
pub enum AstStatementKind {
  #[default]
  ErrAstStatementKind,
  Let(AstLet),
  Expression(AstValExpr),
  Item(AstItem),
}
