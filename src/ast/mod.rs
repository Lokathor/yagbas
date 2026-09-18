//! Module for the Abstract Syntax Tree types.

use str_id::StrId;

use crate::{
  Span,
  ast::{actions::read_module, parser::AstParser},
  cst::{CstElem, actions::gather_module, parser::CstParser},
};

mod actions;
mod parser;

#[derive(Debug, Clone, Default)]
pub struct Ast {
  pub modules: Vec<AstModule>,
  pub errors: Vec<AstError>,
}

#[derive(Debug, Clone)]
pub enum AstError {
  ErrGeneric(String),
}

#[derive(Debug, Clone, Default)]
pub struct AstModule {
  pub file_origin: StrId,
  pub items: Vec<AstItem>,
}
impl AstModule {
  pub fn from_source(file_origin: StrId, src: &str) -> (Self, Vec<AstError>) {
    let mut p = CstParser::new(src);
    gather_module(&mut p);
    let cst = p.build_tree();
    let mut p = AstParser { file_origin, errors: Vec::new() };
    let module = read_module(&mut p, &cst);
    (module, p.errors)
  }
}

#[derive(Debug, Clone, Default)]
pub struct AstItem {
  pub file_origin: StrId,
  pub span: Span,
  pub name: StrId,
  pub name_span: StrId,
  pub kind: AstItemKind,
}

#[derive(Debug, Clone, Default)]
pub enum AstItemKind {
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
  pub items: Vec<AstItem>,
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
  Item(AstItem),
}

#[derive(Debug, Clone, Default)]
pub struct AstLetData {
  pub pattern: AstPatternKind,
  pub ty: Option<AstExprType>,
  pub init: Option<AstExprValue>,
}
