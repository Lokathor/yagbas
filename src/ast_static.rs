use super::*;

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
