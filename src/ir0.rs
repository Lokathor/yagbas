use super::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct TypeID(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeKind {
  Unit,
  Bool,
  U8,
  I8,
  U16,
  I16,
  Array(TypeID, usize),
  Bitbag(IrBitbag),
  Struct(IrStruct),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IrBitbag {
  pub name: StrID,
  pub name_span: Span32,
  pub file_id: FileID,
  /// field names for each bit in the bag
  ///
  /// if a field is multi-bit the name should be stored in *each* position.
  pub fields: [Option<StrID>; 8],
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IrStruct {
  pub name: StrID,
  pub name_span: Span32,
  pub file_id: FileID,
  pub fields: Vec<(StrID, TypeID)>,
}
