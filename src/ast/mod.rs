//! Module for the Abstract Syntax Tree types.

use str_id::StrId;

use crate::{
  Span,
  ast::parser::AstParser,
  cst::{Cst, CstKind},
  ir_nameres::NameId,
  operators::{BinOpKind, UnOpKind},
};

pub mod parser;

#[derive(Debug, Clone, Default)]
pub struct Ast {
  pub modules: Vec<AstModule>,
}

#[derive(Debug, Clone)]
pub struct AstModule {
  pub file_origin: StrId,
  pub items: Vec<AstItem>,
}
impl AstModule {
  pub fn from_source(origin: StrId, src: &str) -> Self {
    let cst = Cst::from_module_src(src);
    debug_assert_eq!(cst.kind, CstKind::Module);
    let ast_parser = AstParser { src: src.to_string() };
    ast_parser.parse_module(origin, &cst)
  }
  pub fn has_errors(&self) -> bool {
    self.items.iter().any(|i| i.has_errors())
  }
}

#[derive(Debug, Clone, Default)]
pub struct AstItem {
  pub span: Span,
  pub kind: AstItemKind,
}
impl AstItem {
  fn has_errors(&self) -> bool {
    self.span == Span::default()
      || match &self.kind {
        AstItemKind::ErrAstItemKind => true,
        AstItemKind::StaticMmio(x) => x.has_errors(),
        AstItemKind::Constant(x) => x.has_errors(),
        AstItemKind::Function(x) => x.has_errors(),
      }
  }
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
  pub span: Span,
  pub kind: AstExprValKind,
}
impl AstExprVal {
  fn has_errors(&self) -> bool {
    self.span == Span::default()
      || match &self.kind {
        AstExprValKind::ErrAstValExprKind => true,
        AstExprValKind::LiteralNumber(str_id) => str_id == &StrId::default(),
        AstExprValKind::Identifier(str_id) => str_id == &StrId::default(),
        AstExprValKind::UnOp(_, inner) => inner.has_errors(),
        AstExprValKind::BinOp(data) => data.has_errors(),
        AstExprValKind::Break => false,
        AstExprValKind::Loop(ast_body) => ast_body.has_errors(),
        AstExprValKind::If(data) => data.has_errors(),
        AstExprValKind::For(data) => data.has_errors(),
        AstExprValKind::ResolvedName(_) => false,
      }
  }
}

#[derive(Debug, Clone, Default)]
pub enum AstExprValKind {
  #[default]
  ErrAstValExprKind,
  //
  Identifier(StrId),
  LiteralNumber(StrId),
  //
  Break,
  If(Box<AstIfData>),
  Loop(Box<AstBody>),
  For(Box<AstForData>),
  //
  UnOp(UnOpKind, Box<AstExprVal>),
  BinOp(Box<AstBinOpData>),
  //
  ResolvedName(NameId),
}

#[derive(Debug, Clone, Default)]
pub struct AstIfData {
  pub condition: AstExprVal,
  pub if_body: AstBody,
  pub else_body: AstBody,
}
impl AstIfData {
  pub fn has_errors(&self) -> bool {
    self.condition.has_errors()
      || self.if_body.has_errors()
      || self.else_body.has_errors()
  }
}

#[derive(Debug, Clone, Default)]
pub struct AstForData {
  pub step_expr: AstExprVal,
  pub range_expr: AstExprVal,
  pub body: AstBody,
}
impl AstForData {
  pub fn has_errors(&self) -> bool {
    self.step_expr.has_errors()
      || self.range_expr.has_errors()
      || self.body.has_errors()
  }
}

#[derive(Debug, Clone, Default)]
pub struct AstBinOpData {
  pub op: BinOpKind,
  pub left: AstExprVal,
  pub right: AstExprVal,
}
impl AstBinOpData {
  pub fn has_errors(&self) -> bool {
    self.op == BinOpKind::ErrBinOpKind
      || self.left.has_errors()
      || self.right.has_errors()
  }
}

#[derive(Debug, Clone, Default)]
pub struct AstExprType {
  pub span: Span,
  pub kind: AstExprTypeKind,
}
impl AstExprType {
  fn has_errors(&self) -> bool {
    self.span == Span::default()
      || match &self.kind {
        AstExprTypeKind::ErrAstTypeExprKind => true,
        AstExprTypeKind::Plain(str_id) => str_id == &StrId::default(),
        AstExprTypeKind::Array { element_ty, length } => {
          element_ty.has_errors() || length.has_errors()
        }
      }
  }
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
  pub name_span: Span,
  pub ty: AstExprType,
}
impl AstStaticMmio {
  fn has_errors(&self) -> bool {
    self.address.has_errors()
      || self.name == StrId::default()
      || self.name_span == Span::default()
      || self.ty.has_errors()
  }
}

#[derive(Debug, Clone, Default)]
pub struct AstConstant {
  pub name: StrId,
  pub name_span: Span,
  pub ty: AstExprType,
  pub xpr: AstExprVal,
}
impl AstConstant {
  fn has_errors(&self) -> bool {
    self.name == StrId::default()
      || self.name_span == Span::default()
      || self.ty.has_errors()
      || self.xpr.has_errors()
  }
}

#[derive(Debug, Clone, Default)]
pub struct AstFunction {
  pub name: StrId,
  pub name_span: Span,
  pub arguments: Vec<AstFunctionArgument>,
  /// if no return is explicit, it's implicitly still `()`
  pub return_ty: AstExprType,
  pub body: AstBody,
}
impl AstFunction {
  fn has_errors(&self) -> bool {
    self.name == StrId::default()
      || self.name_span == Span::default()
      || self.arguments.iter().any(|arg| arg.has_errors())
      || self.return_ty.has_errors()
      || self.body.has_errors()
  }
}

#[derive(Debug, Clone)]
pub struct AstFunctionArgument {
  pub name: StrId,
  pub name_span: Span,
  pub ty: AstExprType,
}
impl AstFunctionArgument {
  fn has_errors(&self) -> bool {
    self.name == StrId::default()
      || self.name_span == Span::default()
      || self.ty.has_errors()
  }
}

#[derive(Debug, Clone, Default)]
pub struct AstBody {
  pub statements: Vec<AstStatement>,
}
impl AstBody {
  fn has_errors(&self) -> bool {
    self.statements.iter().any(|st| st.has_errors())
  }
}

#[derive(Debug, Clone)]
pub struct AstLet {
  pub pattern: AstExprVal,
  pub xpr: AstExprVal,
}
impl AstLet {
  fn has_errors(&self) -> bool {
    self.pattern.has_errors() || self.xpr.has_errors()
  }
}

#[derive(Debug, Clone, Default)]
pub struct AstStatement {
  pub span: Span,
  pub kind: AstStatementKind,
}
impl AstStatement {
  fn has_errors(&self) -> bool {
    self.span == Span::default()
      || match &self.kind {
        AstStatementKind::ErrAstStatementKind => true,
        AstStatementKind::Let(ast_let) => ast_let.has_errors(),
        AstStatementKind::Expression(ast_expr_val) => ast_expr_val.has_errors(),
        AstStatementKind::Item(ast_item) => ast_item.has_errors(),
      }
  }
}

#[derive(Debug, Clone, Default)]
pub enum AstStatementKind {
  #[default]
  ErrAstStatementKind,
  Let(AstLet),
  Expression(AstExprVal),
  Item(AstItem),
}
