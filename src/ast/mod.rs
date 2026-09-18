//! Module for the Abstract Syntax Tree types.

use str_id::StrId;

use crate::{Span, cst::CstElem, make_key};

pub mod actions;
pub mod parser;

#[derive(Debug, Clone, Default)]
pub struct Ast {
  pub items: Vec<Item>,
  pub errors: Vec<AstError>,
}

#[derive(Debug, Clone)]
pub enum AstError {
  ErrGeneric(String),
}

make_key!(ItemId);

#[derive(Debug, Clone, Default)]
pub struct Item {
  pub file_origin: StrId,
  pub span: Span,
  pub name: StrId,
  pub name_span: StrId,
  pub kind: ItemKind,
}

#[derive(Debug, Clone, Default)]
pub enum ItemKind {
  #[default]
  ErrDefault,
  Constant(AstConstant),
  Static(AstStatic),
  Function(AstFunction),
  Struct(AstStruct),
  BitBag(AstBitBag),
  Enum(AstEnum),
  Impl(AstImpl),
  Use(AstUse),
}

#[derive(Debug, Clone, Default)]
pub struct AstConstant {
  pub ty: AstExprType,
  pub expr: AstExprValue,
}

#[derive(Debug, Clone, Default)]
pub struct AstStatic {
  pub ty: AstExprType,
  pub kind: AstStaticKind,
}

#[derive(Debug, Clone, Default)]
pub struct AstFunction {
  pub args: Vec<AstFunctionArg>,
  pub return_ty: AstExprType,
  pub body: AstBody,
}

#[derive(Debug, Clone, Default, Copy)]
pub struct AstFunctionArg {
  pub pattern: AstPatternKind,
  pub ty: AstExprType,
}

#[derive(Debug, Clone, Default)]
pub struct AstStruct {
  pub fields: Vec<AstStructField>,
}

#[derive(Debug, Clone, Default)]
pub struct AstEnum {
  pub variants: Vec<StrId>,
}

#[derive(Debug, Clone, Default, Copy)]
pub struct AstStructField {
  pub name: StrId,
  pub name_span: StrId,
  pub ty: AstExprType,
}

#[derive(Debug, Clone, Default)]
pub struct AstBitBag {
  pub fields: Vec<AstBitBagField>,
}

#[derive(Debug, Clone, Default)]
pub struct AstBitBagField {
  pub name: StrId,
  pub name_span: StrId,
  pub position: AstExprValue,
}

#[derive(Debug, Clone, Default, Copy)]
pub enum AstPatternKind {
  #[default]
  ErrDefault,
  Simple {
    name: StrId,
    span: Span,
  },
}

#[derive(Debug, Clone, Default)]
pub enum AstStaticKind {
  #[default]
  ErrDefault,
  Ram(AstExprValue),
  Rom(AstExprValue),
  Mmio(AstExprValue),
}

#[derive(Debug, Clone, Default, Copy)]
pub enum AstExprType {
  #[default]
  ErrDefault,
  // TODO
}

#[derive(Debug, Clone, Default)]
pub enum AstExprValue {
  #[default]
  ErrDefault,
  Body(Box<AstBody>),
  // TODO
}

#[derive(Debug, Clone, Default)]
pub struct AstImpl {
  pub target: AstExprType,
  pub items: Vec<Item>,
}

#[derive(Debug, Clone, Default)]
pub struct AstUse {
  pub cst: Vec<CstElem>,
}

#[derive(Debug, Clone, Default)]
pub struct AstBody {
  pub statements: Vec<AstStatement>,
}

#[derive(Debug, Clone, Default)]
pub enum AstStatement {
  #[default]
  ErrDefault,
  Let(AstLetData),
  Expression(AstExprValue),
  Item(Item),
}

#[derive(Debug, Clone, Default)]
pub struct AstLetData {
  pub pattern: AstPatternKind,
  pub ty: Option<AstExprType>,
  pub init: Option<AstExprValue>,
}
