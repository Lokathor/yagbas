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
pub struct AstExprVal {
  pub span: Range<usize>,
  pub kind: AstExprValKind,
}

#[derive(Debug, Clone, Default)]
pub enum AstExprValKind {
  #[default]
  ErrAstValExprKind,
  LiteralNumber(StrId),
  Identifier(StrId),
  Reference(Box<AstExprVal>),
  Dereference(Box<AstExprVal>),
  Mul(Box<AstExprVal>, Box<AstExprVal>),
  CmpEq(Box<AstExprVal>, Box<AstExprVal>),
  Assign(Box<AstExprVal>, Box<AstExprVal>),
  AddAssign(Box<AstExprVal>, Box<AstExprVal>),
  ArrayIndex(Box<AstExprVal>, Box<AstExprVal>),
  RangeInclusive(Box<AstExprVal>, Box<AstExprVal>),
  RangeExclusive(Box<AstExprVal>, Box<AstExprVal>),
  Loop(Box<AstBody>),
  If(Box<AstExprVal>, Box<AstBody>),
  Break,
  For(Box<AstExprVal>, Box<AstExprVal>, Box<AstBody>),
  Path(Box<AstExprVal>, Box<AstExprVal>),
  Access(Box<AstExprVal>, Box<AstExprVal>),
  Div(Box<AstExprVal>, Box<AstExprVal>),
  Rem(Box<AstExprVal>, Box<AstExprVal>),
  Add(Box<AstExprVal>, Box<AstExprVal>),
  Sub(Box<AstExprVal>, Box<AstExprVal>),
  ShiftLeft(Box<AstExprVal>, Box<AstExprVal>),
  ShiftRight(Box<AstExprVal>, Box<AstExprVal>),
  BitAnd(Box<AstExprVal>, Box<AstExprVal>),
  BitOr(Box<AstExprVal>, Box<AstExprVal>),
  BitXor(Box<AstExprVal>, Box<AstExprVal>),
  CmpNe(Box<AstExprVal>, Box<AstExprVal>),
  CmpLt(Box<AstExprVal>, Box<AstExprVal>),
  CmpGt(Box<AstExprVal>, Box<AstExprVal>),
  CmpLe(Box<AstExprVal>, Box<AstExprVal>),
  CmpGe(Box<AstExprVal>, Box<AstExprVal>),
  ConditionalAnd(Box<AstExprVal>, Box<AstExprVal>),
  ConditionalOr(Box<AstExprVal>, Box<AstExprVal>),
  SubAssign(Box<AstExprVal>, Box<AstExprVal>),
  MulAssign(Box<AstExprVal>, Box<AstExprVal>),
  DivAssign(Box<AstExprVal>, Box<AstExprVal>),
  RemAssign(Box<AstExprVal>, Box<AstExprVal>),
  BitAndAssign(Box<AstExprVal>, Box<AstExprVal>),
  BitOrAssign(Box<AstExprVal>, Box<AstExprVal>),
  BitXorAssign(Box<AstExprVal>, Box<AstExprVal>),
  ShiftLeftAssign(Box<AstExprVal>, Box<AstExprVal>),
  ShiftRightAssign(Box<AstExprVal>, Box<AstExprVal>),
}

#[derive(Debug, Clone, Default)]
pub struct AstExprType {
  pub span: Range<usize>,
  pub kind: AstExprTypeKind,
}

#[derive(Debug, Clone, Default)]
pub enum AstExprTypeKind {
  #[default]
  ErrAstTypeExprKind,
  Plain(StrId),
  Array {
    element_ty: Box<AstExprType>,
    length: Box<AstExprVal>,
  },
}

#[derive(Debug, Clone, Default)]
pub struct AstStaticMmio {
  pub address: AstExprVal,
  pub name: StrId,
  pub name_span: Range<usize>,
  pub ty: AstExprType,
}

#[derive(Debug, Clone, Default)]
pub struct AstConstant {
  pub name: StrId,
  pub name_span: Range<usize>,
  pub ty: AstExprType,
  pub xpr: AstExprVal,
}

#[derive(Debug, Clone, Default)]
pub struct AstFunction {
  pub name: StrId,
  pub name_span: Range<usize>,
  pub arguments: Vec<AstFunctionArgument>,
  /// if no return is explicit, it's implicitly still `()`
  pub return_ty: AstExprType,
  pub body: AstBody,
}

#[derive(Debug, Clone)]
pub struct AstFunctionArgument {
  pub name: Option<StrId>,
  pub name_span: Range<usize>,
  pub ty: AstExprType,
  pub ty_span: Range<usize>,
}

#[derive(Debug, Clone, Default)]
pub struct AstBody {
  pub statements: Vec<AstStatement>,
}

#[derive(Debug, Clone)]
pub struct AstLet {
  pub pattern: AstExprVal,
  pub xpr: AstExprVal,
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
  Expression(AstExprVal),
  Item(AstItem),
}
