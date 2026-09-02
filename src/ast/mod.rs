//! Module for the Abstract Syntax Tree types.

use std::ops::Range;
use str_id::StrId;

use crate::{
  ast::parser::AstParser,
  cst::{Cst, CstKind},
};

pub mod parser;

#[derive(Debug, Clone)]
pub struct Ast {
  pub modules: Vec<AstModule>,
}

#[derive(Debug, Clone)]
pub struct AstModule {
  pub items: Vec<AstItem>,
  // TODO: add a field to track where the module came from.
}
impl AstModule {
  pub fn from_source(src: &str) -> Self {
    let cst = Cst::from_module_src(src);
    debug_assert_eq!(cst.kind, CstKind::Module);
    let ast_parser = AstParser { src: src.to_string() };
    ast_parser.parse_module(&cst)
  }
  pub fn has_errors(&self) -> bool {
    self.items.iter().any(|i| i.has_errors())
  }
}

#[derive(Debug, Clone, Default)]
pub struct AstItem {
  pub span: Range<usize>,
  pub kind: AstItemKind,
}
impl AstItem {
  fn has_errors(&self) -> bool {
    self.span == (0..0)
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
  pub span: Range<usize>,
  pub kind: AstExprValKind,
}
impl AstExprVal {
  fn has_errors(&self) -> bool {
    self.span == (0..0)
      || match &self.kind {
        AstExprValKind::ErrAstValExprKind => true,
        AstExprValKind::LiteralNumber(str_id) => str_id == &StrId::default(),
        AstExprValKind::Identifier(str_id) => str_id == &StrId::default(),
        AstExprValKind::Reference(ast_expr_val) => ast_expr_val.has_errors(),
        AstExprValKind::Dereference(ast_expr_val) => ast_expr_val.has_errors(),
        AstExprValKind::Break => false,
        AstExprValKind::Loop(ast_body) => ast_body.has_errors(),
        AstExprValKind::If(ast_expr_val, ast_body) => {
          ast_expr_val.has_errors() || ast_body.has_errors()
        }
        AstExprValKind::For(ast_expr_val, ast_expr_val1, ast_body) => {
          ast_expr_val.has_errors()
            || ast_expr_val1.has_errors()
            || ast_body.has_errors()
        }
        AstExprValKind::Mul(ast_expr_val, ast_expr_val1)
        | AstExprValKind::CmpEq(ast_expr_val, ast_expr_val1)
        | AstExprValKind::Assign(ast_expr_val, ast_expr_val1)
        | AstExprValKind::AddAssign(ast_expr_val, ast_expr_val1)
        | AstExprValKind::ArrayIndex(ast_expr_val, ast_expr_val1)
        | AstExprValKind::RangeInclusive(ast_expr_val, ast_expr_val1)
        | AstExprValKind::RangeExclusive(ast_expr_val, ast_expr_val1)
        | AstExprValKind::Path(ast_expr_val, ast_expr_val1)
        | AstExprValKind::Access(ast_expr_val, ast_expr_val1)
        | AstExprValKind::Div(ast_expr_val, ast_expr_val1)
        | AstExprValKind::Rem(ast_expr_val, ast_expr_val1)
        | AstExprValKind::Add(ast_expr_val, ast_expr_val1)
        | AstExprValKind::Sub(ast_expr_val, ast_expr_val1)
        | AstExprValKind::ShiftLeft(ast_expr_val, ast_expr_val1)
        | AstExprValKind::ShiftRight(ast_expr_val, ast_expr_val1)
        | AstExprValKind::BitAnd(ast_expr_val, ast_expr_val1)
        | AstExprValKind::BitOr(ast_expr_val, ast_expr_val1)
        | AstExprValKind::BitXor(ast_expr_val, ast_expr_val1)
        | AstExprValKind::CmpNe(ast_expr_val, ast_expr_val1)
        | AstExprValKind::CmpLt(ast_expr_val, ast_expr_val1)
        | AstExprValKind::CmpGt(ast_expr_val, ast_expr_val1)
        | AstExprValKind::CmpLe(ast_expr_val, ast_expr_val1)
        | AstExprValKind::CmpGe(ast_expr_val, ast_expr_val1)
        | AstExprValKind::ConditionalAnd(ast_expr_val, ast_expr_val1)
        | AstExprValKind::ConditionalOr(ast_expr_val, ast_expr_val1)
        | AstExprValKind::SubAssign(ast_expr_val, ast_expr_val1)
        | AstExprValKind::MulAssign(ast_expr_val, ast_expr_val1)
        | AstExprValKind::DivAssign(ast_expr_val, ast_expr_val1)
        | AstExprValKind::RemAssign(ast_expr_val, ast_expr_val1)
        | AstExprValKind::BitAndAssign(ast_expr_val, ast_expr_val1)
        | AstExprValKind::BitOrAssign(ast_expr_val, ast_expr_val1)
        | AstExprValKind::BitXorAssign(ast_expr_val, ast_expr_val1)
        | AstExprValKind::ShiftLeftAssign(ast_expr_val, ast_expr_val1)
        | AstExprValKind::ShiftRightAssign(ast_expr_val, ast_expr_val1) => {
          ast_expr_val.has_errors() || ast_expr_val1.has_errors()
        }
      }
  }
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
impl AstExprType {
  fn has_errors(&self) -> bool {
    self.span == (0..0)
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
  pub name_span: Range<usize>,
  pub ty: AstExprType,
}
impl AstStaticMmio {
  fn has_errors(&self) -> bool {
    self.address.has_errors()
      || self.name == StrId::default()
      || self.name_span == (0..0)
      || self.ty.has_errors()
  }
}

#[derive(Debug, Clone, Default)]
pub struct AstConstant {
  pub name: StrId,
  pub name_span: Range<usize>,
  pub ty: AstExprType,
  pub xpr: AstExprVal,
}
impl AstConstant {
  fn has_errors(&self) -> bool {
    self.name == StrId::default()
      || self.name_span == (0..0)
      || self.ty.has_errors()
      || self.xpr.has_errors()
  }
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
impl AstFunction {
  fn has_errors(&self) -> bool {
    self.name == StrId::default()
      || self.name_span == (0..0)
      || self.arguments.iter().any(|arg| arg.has_errors())
      || self.return_ty.has_errors()
      || self.body.has_errors()
  }
}

#[derive(Debug, Clone)]
pub struct AstFunctionArgument {
  pub name: StrId,
  pub name_span: Range<usize>,
  pub ty: AstExprType,
}
impl AstFunctionArgument {
  fn has_errors(&self) -> bool {
    self.name == StrId::default()
      || self.name_span == (0..0)
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
  pub span: Range<usize>,
  pub kind: AstStatementKind,
}
impl AstStatement {
  fn has_errors(&self) -> bool {
    self.span == (0..0)
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
