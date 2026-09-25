//! Module for the Abstract Syntax Tree types.

use crate::ValueExprId;
use crate::{
  ItemId, LabelId, LocalNameId, Span, YagError,
  cst::Cst,
  operators::{BinOpKind, UnOpKind},
  path_id::PathId,
};

pub mod actions;
pub mod parser;
pub mod visitor;

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
  Constant(ConstantData),
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
  Function(FunctionData),
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
pub struct ConstantData {
  pub type_decl: TypeExpr,
  pub value_decl: ValueExpr,
}

#[derive(Debug, Clone, Default)]
pub struct FunctionData {
  pub args: Vec<FunctionArg>,
  pub opt_ret_tyx: Option<TypeExpr>,
  pub body: ValueExpr,
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
pub struct Label {
  pub span: Span,
  pub kind: LabelKind,
}

#[derive(Debug, Clone, Default)]
pub enum LabelKind {
  #[default]
  ErrLabelKind,
  Identifier(String),
  IdNum(LabelId),
}

#[derive(Debug, Clone, Default)]
pub struct FunctionArg {
  pub var: ValueExpr,
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
  Identifier(String),
  Array {
    elem_tyx: TypeExpr,
    elem_count: ValueExpr,
  },
  /// `*access target`
  Pointer {
    target_tyx: TypeExpr,
    access_kind: PointerAccessKind,
  },
  NameOfStruct(ItemId),
  NameOfBitbag(ItemId),
  NameOfEnum(ItemId),
  /// `()`
  Unit,
  /// `bool`
  Bool,
  U8,
  I8,
  U16,
  I16,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PointerAccessKind {
  /// Constant data, read only.
  Const,
  /// Mutable data, read/write.
  Mut,
  /// Volatile data, read/write and access cannot be elided.
  Vol,
}

#[derive(Debug, Clone)]
pub struct ValueExpr {
  pub span: Span,
  pub id: ValueExprId,
  pub kind: Box<ValueExprKind>,
}
impl Default for ValueExpr {
  fn default() -> Self {
    Self {
      span: Span::default(),
      id: ValueExprId::new(),
      kind: Box::new(ValueExprKind::default()),
    }
  }
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
    label: Option<Label>,
    body: ValueExpr,
  },
  While {
    label: Option<Label>,
    condition: ValueExpr,
    body: ValueExpr,
  },
  /// For loops are sugar for the following:
  /// ```txt
  /// {
  ///   let step_var = range.start;
  ///   'label: while step_var {range_op} range.end {
  ///     // loop statements
  ///     step_var += 1;
  ///   }
  /// }
  /// ```
  /// where {range_op} is
  /// * `<` for exclusive ranges
  /// * `<=` for inclusive ranges.
  For {
    label: Option<Label>,
    step_var: ValueExpr,
    range: ValueExpr,
    body: ValueExpr,
  },
  If {
    condition: ValueExpr,
    true_body: ValueExpr,
    opt_false_body: Option<ValueExpr>,
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
    label: Option<Label>,
    value: Option<ValueExpr>,
  },
  Continue {
    label: Option<Label>,
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
  NameOfStaticRam(ItemId),
  NameOfStaticRom(ItemId),
  NameOfConstant(ItemId),
  NameOfFunction(ItemId),
  NameOfLocalVariable(LocalNameId),
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
    var: ValueExpr,
    type_decl: Option<TypeExpr>,
    initializer: Option<ValueExpr>,
  },
  Expression(ValueExpr),
}
