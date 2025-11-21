use super::*;

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub enum AstAttribute {
  #[default]
  AttributeError,
  /// looks like `#[hram]`
  Ident(StrID, Span32),
  /// looks like `#[game_revision = 2]`
  Assignment(StrID, Span32, Expr),
  /// looks like `#[location($FF00)]`
  Call(StrID, Span32, Vec<AstAttribute>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AstBitstruct {
  pub span: Span32,
  pub name: StrID,
  pub name_span: Span32,
  pub attributes: Vec<AstAttribute>,
  pub fields: Vec<AstBitstructFieldDef>,
  pub file_id: FileID,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AstBitstructFieldDef {
  pub span: Span32,
  pub name: StrID,
  pub name_span: Span32,
  pub bit: Expr,
  pub bit_span: Span32,
  pub attributes: Vec<AstAttribute>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AstConst {
  pub span: Span32,
  pub name: StrID,
  pub name_span: Span32,
  pub ty: StrID,
  pub ty_span: Span32,
  pub expr: Expr,
  pub expr_span: Span32,
  pub attributes: Vec<AstAttribute>,
  pub file_id: FileID,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AstFunction {
  span: Span32,
  file_id: FileID,
  name: StrID,
  name_span: Span32,
  args: Vec<AstFunctionArg>,
  return_ty: StrID,
  return_ty_span: Span32,
  body: Vec<Statement>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AstFunctionArg {
  name: StrID,
  name_span: Span32,
  ty: StrID,
  ty_span: Span32,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AstStatic {
  pub span: Span32,
  pub name: StrID,
  pub name_span: Span32,
  pub ty: StrID,
  pub ty_span: Span32,
  pub expr: Expr,
  pub expr_span: Span32,
  pub attributes: Vec<AstAttribute>,
  pub file_id: FileID,
  pub memory_kind: MemoryKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MemoryKind {
  /// Data in an immutable ROM region.
  Rom,
  /// Data in a "normal" mutable RAM region.
  ///
  /// Repeated reads/writes to the same RAM location inside of a function are
  /// allowed to be collapsed to a single access.
  Ram,
  /// Data that is Memory-mapped IO.
  ///
  /// All accesses to MMIO are what Rust/LLVM call "volatile". Every read/write
  /// needs to actually happen. Repeated writes or reads of the same location
  /// cannot be combined.
  MemoryMappedIO,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AstStruct {
  pub span: Span32,
  pub name: StrID,
  pub name_span: Span32,
  pub attributes: Vec<AstAttribute>,
  pub fields: Vec<AstStructFieldDef>,
  pub file_id: FileID,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AstStructFieldDef {
  pub span: Span32,
  pub name: StrID,
  pub name_span: Span32,
  pub ty: StrID,
  pub ty_span: Span32,
  pub attributes: Vec<AstAttribute>,
}
