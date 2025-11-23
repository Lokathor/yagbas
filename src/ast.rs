use super::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AstItem {
  file_id: FileID,
  span: Span32,
  attributes: Vec<AstAttribute>,
  kind: AstItemKind,
}

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
pub enum AstItemKind {
  ItemKindError,
  Bitbag(AstBitbag),
  Struct(AstStruct),
  Const(AstConst),
  Static(AstStatic),
  Function(AstFunction),
}

/// Declares a type with field names assigned to bit positions.
///
/// ```yag
/// bitbag NameHere {
///   one_field: 0,
///   another_field: 7,
/// }
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AstBitbag {
  pub name: StrID,
  pub name_span: Span32,
  pub fields: Vec<AstBitbagFieldDef>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AstBitbagFieldDef {
  pub span: Span32,
  /// we want to allow attribues mostly so that we can allow `cfg`.
  pub attributes: Vec<AstAttribute>,
  pub name: StrID,
  pub name_span: Span32,
  pub bit: Expr,
  pub bit_span: Span32,
}

/// Declares a type where each field is some other type of data.
///
/// ```yag
/// struct NameHere {
///   one_field: u8,
///   another_field: i8,
/// }
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AstStruct {
  pub name: StrID,
  pub name_span: Span32,
  pub fields: Vec<AstStructFieldDef>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AstStructFieldDef {
  pub span: Span32,
  /// we want to allow attribues mostly so that we can allow `cfg`.
  pub attributes: Vec<AstAttribute>,
  pub name: StrID,
  pub name_span: Span32,
  pub ty: StrID,
  pub ty_span: Span32,
}

/// Gives a name to a constant expression.
///
/// ```yag
/// const NAME_HERE: Ty = Expression;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AstConst {
  pub name: StrID,
  pub name_span: Span32,
  pub ty: StrID,
  pub ty_span: Span32,
  pub expr: Expr,
  pub expr_span: Span32,
}

/// A callable body of code within the program.
///
/// ```yag
/// fn name_here(args:ArgTy, arg1:ArgTy1, ...) -> ReturnTy {
///   statement;
///   statement;
///   // ..
///   return_value
/// }
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AstFunction {
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

/// Static data within the program.
///
/// ```yag
/// static memory_kind NAME_HERE: Ty = InitializationExpression;
/// ```
///
/// * `static rom` is immutable data, such as tile or tilemap data.
/// * `static ram` is plain mutable data, such as a current position.
/// * `static mmio` is volatile mutable data, for memory-mapped IO.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AstStatic {
  pub memory_kind: MemoryKind,
  pub name: StrID,
  pub name_span: Span32,
  pub ty: StrID,
  pub ty_span: Span32,
  pub expr: Expr,
  pub expr_span: Span32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MemoryKind {
  Rom,
  Ram,
  MemoryMappedIO,
}
