//! Module for the Abstract Syntax Tree types.

use std::path::PathBuf;

use crate::{
  Span,
  cst::Cst,
  operators::{BinOpKind, UnOpKind},
};

pub mod actions;

#[derive(Debug, Clone, Default)]
pub struct Ast {
  pub modules: Vec<Module>,
  pub errors: Vec<AstError>,
}

#[derive(Debug, Clone, Default)]
pub struct Module {
  pub file_origin: PathBuf,
  pub items: Vec<Item>,
}

#[derive(Debug, Clone)]
pub enum AstError {
  ErrGeneric(Span, String),
  CstParserMadeModuleWithBadData(String),
}

#[derive(Debug, Clone, Default)]
pub struct Item {
  pub file_origin: PathBuf,
  pub span: Span,
  pub name: String,
  pub name_span: Span,
  pub kind: ItemKind,
}

#[derive(Debug, Clone, Default)]
pub enum ItemKind {
  #[default]
  ErrItemKind,
  Constant {
    type_decl: TypeExpr,
    value_decl: ValueExpr,
  },
  Static {
    kind: StaticKind,
  },
  Function {
    args: Vec<FunctionArg>,
    ret_ty: TypeExpr,
    statements: Vec<Statement>,
  },
  Struct {
    fields: Vec<StructField>,
  },
  Bitbag {
    fields: Vec<BitbagField>,
  },
  Enum {
    variants: Vec<String>,
  },
  Impl {
    target: TypeExpr,
    items: Vec<Item>,
  },
  /// I have no idea how to better organize the data from a `use`, and it's not
  /// really that important right now.
  Use {
    cst: Cst,
  },
  /// The name of the module is all we need to know, so there's no extra data.
  Mod,
}

#[derive(Debug, Clone, Default)]
pub struct BitbagField {
  pub name: String,
  pub name_span: Span,
  pub bit: ValueExpr,
}

#[derive(Debug, Clone, Default)]
pub struct StructField {
  pub name: String,
  pub name_span: Span,
  pub type_decl: TypeExpr,
}

#[derive(Debug, Clone, Default)]
pub struct Pattern {
  pub span: Span,
  pub kind: PatternKind,
}

#[derive(Debug, Clone, Default)]
pub enum PatternKind {
  #[default]
  ErrPatternKind,
  Simple(String),
}

#[derive(Debug, Clone, Default)]
pub struct FunctionArg {
  pub pattern: Pattern,
  pub type_decl: TypeExpr,
}

#[derive(Debug, Clone, Default)]
pub enum StaticKind {
  #[default]
  ErrStaticKind,
  Mmio {
    location: ValueExpr,
    type_decl: TypeExpr,
  },
  Ram {
    type_decl: TypeExpr,
    init: ValueExpr,
  },
  Rom {
    type_decl: TypeExpr,
    data: ValueExpr,
  },
}

#[derive(Debug, Clone, Default)]
pub struct TypeExpr {
  pub span: Span,
  pub kind: Box<TypeExprKind>,
}
#[derive(Debug, Clone, Default)]
pub enum TypeExprKind {
  #[default]
  ErrTypeExprKind,
  Simple(String),
  Array {
    elem_ty: TypeExpr,
    elem_count: ValueExpr,
  },
  Pointer {
    elem_ty: TypeExpr,
    access_kind: PointerAccessKind,
  },
}

#[derive(Debug, Clone, Copy)]
pub enum PointerAccessKind {
  /// Constant data, read only.
  Const,
  /// Mutable data, read/write.
  Mut,
  /// Volatile data, read/write and access cannot be elided.
  Vol,
}

#[derive(Debug, Clone, Default)]
pub struct ValueExpr {
  pub span: Span,
  pub kind: Box<ValueExprKind>,
}
#[derive(Debug, Clone, Default)]
pub enum ValueExprKind {
  #[default]
  ErrValueExprKind,
  /// Unresolved identifier for something.
  Identifier(String),
  /// Literal string token text.
  LiteralString(String),
  /// Literal number token text.
  LiteralNumber(String),
  Body {
    statements: Vec<Statement>,
  },
  Loop {
    label: Option<String>,
    statements: Vec<Statement>,
  },
  While {
    label: Option<String>,
    condition: ValueExpr,
    statements: Vec<Statement>,
  },
  For {
    label: Option<String>,
    step_var: Pattern,
    range: ValueExpr,
    statements: Vec<Statement>,
  },
  If {
    condition: ValueExpr,
    when_true: Vec<Statement>,
    when_false: Vec<Statement>,
  },
  BinOp {
    left: ValueExpr,
    op: BinOpKind,
    right: ValueExpr,
  },
  UnOp {
    op: UnOpKind,
    operand: ValueExpr,
  },
  Break {
    label: Option<String>,
    value: Option<ValueExpr>,
  },
  Continue {
    label: Option<String>,
  },
  Call {
    target: ValueExpr,
    args: Vec<ValueExpr>,
  },
  /// Probably we should not support `as` ops, and instead make methods for the
  /// different changes instead, but for now we're just "doing what rust does".
  As {
    value: ValueExpr,
    as_type: TypeExpr,
  },
}

#[derive(Debug, Clone, Default)]
pub struct Statement {
  pub span: Span,
  pub kind: Box<StatementKind>,
}
#[derive(Debug, Clone, Default)]
pub enum StatementKind {
  #[default]
  ErrStatementKind,
  Item(Item),
  Let {
    pattern: Pattern,
    type_decl: Option<TypeExpr>,
    initializer: Option<ValueExpr>,
  },
  Expression(ValueExpr),
}
