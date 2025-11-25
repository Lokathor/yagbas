use super::*;

/// An item defines a "stand alone thing" within the program.
///
/// * Currently there's no namespacing of items, so no two items should ever
///   share the same name. This is an annoying limitation on yagbas programmers,
///   so a namespace system will hopefully be added in the future.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AstItem {
  pub file_id: FileID,
  pub span: Span32,
  pub attributes: Vec<AstAttribute>,
  pub name: StrID,
  pub name_span: Span32,
  pub kind: AstItemKind,
}

/// Attributes declare optional metadata or configuration for items and for
/// individutal statements.
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
  pub ty: StrID,
  pub ty_span: Span32,
  pub expr: Expr,
  pub expr_span: Span32,
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
  pub args: Vec<AstFunctionArg>,
  pub return_ty: StrID,
  pub return_ty_span: Span32,
  pub body: Vec<Statement>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AstFunctionArg {
  name: StrID,
  name_span: Span32,
  ty: StrID,
  ty_span: Span32,
}

/// One single line of code in a body of code.
///
/// There's one `Vec<Statement>` per body of code in the AST, so we should be
/// mindful about the size of this type.
#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct Statement {
  pub span: Span32,
  /// most statements have 0 attributes, so we Option this.
  pub attribues: Option<Box<Vec<AstAttribute>>>,
  pub kind: Box<StatementKind>,
}

/// The different kinds of statement that exist.
///
/// Currently there's only two "actual" statement kinds: Let and Expr.
/// * `Let` introduces a new local variable.
/// * `Expr` performs an expression, generally an assignment, call, or loop.
/// * `LetAssign` is semantically identical to Let followed by an Expr(Assign),
///   and this kind can be split up during some early stage of the compiler. It
///   exists as a combination tag so that the parsing is simpler.
#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub enum StatementKind {
  #[default]
  StatementKindError,

  /// looks like `let varname: vartype;`
  Let(StrID, Option<StrID>),

  /// looks like `let varname: vartype = expr;`
  LetAssign(StrID, Option<StrID>, Expr),

  /// Any expression on its own can be a statement.
  Expr(Expr),
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct Expr {
  pub span: Span32,
  pub kind: Box<ExprKind>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub enum ExprKind {
  #[default]
  ExprError,

  NumLit(ExprNumLit),
  Ident(ExprIdent),
  Bool(bool),

  /// `[ ... , ... , ... ]`
  ///
  /// square brackets around sub-expressions that forms a series of elements.
  List(ExprList),

  /// `{ ... ; ... ; ... }`
  ///
  /// braces around a series of statements.
  Block(ExprBlock),

  Call(ExprCall),
  Macro(ExprMacro),
  StructLit(ExprStructLit),

  IfElse(ExprIfElse),
  Loop(ExprLoop),
  LoopTimes(ExprLoopTimes),
  Break(ExprBreak),
  Continue(ExprContinue),
  Return(ExprReturn),

  UnOp(ExprUnOp),
  BinOp(ExprBinOp),
}

#[test]
fn test_expr_size() {
  // note(lokathor): any change in size might be justified (and so we would
  // update this test), but we should still take note of it happening.
  assert_eq!(size_of::<Expr>(), size_of::<[usize; 2]>());
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExprNumLit {
  pub lit: StrID,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExprIdent {
  pub ident: StrID,
}

/// Unary operation expression.
///
/// The span of "this" op ends up covering the inner expression as well as the
/// operator token, while the span of "inner" will naturally be the span of only
/// the inner sub-expression.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExprUnOp {
  pub inner: Expr,
  pub kind: UnOpKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnOpKind {
  /// `*x`
  Deref,
  /// `-x`
  Neg,
  /// `&x`
  Ref,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExprBinOp {
  pub lhs: Expr,
  pub rhs: Expr,
  pub kind: BinOpKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinOpKind {
  Assign,
  Add,
  Sub,
  Mul,
  Div,
  Mod,
  ShiftLeft,
  ShiftRight,
  BitAnd,
  BitOr,
  BitXor,
  BoolAnd,
  BoolOr,
  Index,
  Dot,
  Eq,
  Ne,
  Lt,
  Le,
  Gt,
  Ge,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExprCall {
  pub target: StrID,
  pub target_span: Span32,
  pub args: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExprMacro {
  pub target: StrID,
  pub target_span: Span32,
  pub args: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExprStructLit {
  pub ty: StrID,
  pub ty_span: Span32,
  pub args: Vec<FieldAssign>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub enum FieldAssign {
  #[default]
  FieldAssignError,
  Ident(StrID, Span32),
  IdentEq(StrID, Span32, Expr, Span32),
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct ExprIfElse {
  pub condition: Expr,
  pub if_body: Vec<Statement>,
  pub else_body: Vec<Statement>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct ExprList {
  pub elements: Vec<Expr>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct ExprBlock {
  pub body: Vec<Statement>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct ExprLoop {
  pub name: Option<StrID>,
  pub body: Vec<Statement>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct ExprLoopTimes {
  pub name: Option<StrID>,
  pub times: StrID,
  pub times_span: Span32,
  pub body: Vec<Statement>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct ExprBreak {
  pub target: Option<StrID>,
  pub target_span: Span32,
  pub val: Option<Expr>,
  pub val_span: Span32,
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub struct ExprContinue {
  pub target: Option<StrID>,
  pub target_span: Span32,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct ExprReturn {
  pub val: Option<Expr>,
  pub val_span: Span32,
}
