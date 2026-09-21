//! Module for the Abstract Syntax Tree types.

use crate::{
  Span, YagError,
  cst::Cst,
  make_global_id,
  operators::{BinOpKind, UnOpKind},
  path_id::PathId,
};

pub mod actions;
pub mod parser;

#[derive(Debug, Clone, Default)]
pub struct Ast {
  pub modules: Vec<Module>,
  pub errors: Vec<YagError>,
}

#[derive(Debug, Clone, Default)]
pub struct Module {
  pub file_origin: PathId,
  pub items: Vec<Item>,
}

make_global_id!(
  /// Globally unique ID value for a particular [Item].
  ItemId
);

#[derive(Debug, Clone)]
pub struct Item {
  pub file_origin: PathId,
  pub span: Span,
  pub name: String,
  pub name_span: Span,
  pub id: ItemId,
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
  StaticMmio {
    location: ValueExpr,
    type_decl: TypeExpr,
  },
  StaticRam {
    type_decl: TypeExpr,
    init: ValueExpr,
  },
  StaticRom {
    type_decl: TypeExpr,
    data: ValueExpr,
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
  /// `use some::item::path;`
  ///
  /// I have no idea how to better organize the data from a `use`, so for now we
  /// just store the entire [Cst].
  Use {
    cst: Cst,
  },
  /// `mod somename;`
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
  SimpleLocalVariable(u32),
}

#[derive(Debug, Clone, Default)]
pub struct FunctionArg {
  pub pattern: Pattern,
  pub type_decl: TypeExpr,
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
  Block {
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
  /// `..` with no left or right sub-expression
  FullRangeExclusive,
  /// `..=` with no left or right sub-expression
  FullRangeInclusive,
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
  NameOfStaticMmio(ItemId),
  NameOfConstant(ItemId),
  NameOfFunction(ItemId),
  NameOfLocalVariable(u32),
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
