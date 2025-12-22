use crate::*;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AstItem {
  pub file_id: FileID,
  pub span: Span32,
  pub attributes: Vec<Attribute>,
  pub kind: AstItemKind,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AstItemKind {
  AstItemKindError,
  Function(AstFunction),
  Const(AstConst),
  Static(AstStatic),
  Struct(AstStruct),
  Bitbag(AstBitbag),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Attribute {
  pub span: Span32,
  pub kind: AttributeKind,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AttributeKind {
  AttributeKindError,
  Assign(StrID, Span32, Expr),
  Expr(Expr),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeName {
  pub span: Span32,
  pub kind: TypeNameKind,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeNameKind {
  TypeNameKindError,
  Ident(StrID),
  ArrayNumLit(Box<TypeNameKind>, StrID),
  ArrayIdent(Box<TypeNameKind>, StrID),
}

/// A callable body of code within a program.
///
/// ```yag
/// #[attribute]
/// #[...]
/// fn name(arg: arg_ty, ...) -> return_ty {
///   statement;
///   ...;
///   output_expression
/// }
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AstFunction {
  pub name: StrID,
  pub name_span: Span32,
  pub args: Vec<AstFunctionArgKind>,
  pub return_ty: Option<TypeName>,
  pub body: AstSatementBody,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AstFunctionArgKind {
  AstFunctionArgKindError,
  NameTy(StrID, Span32, TypeName),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AstSatementBody {
  pub statements: Vec<Statement>,
  pub tail_expr: Option<Expr>,
}

/// Statements are the steps of a "statement body" that happen before the final
/// expression.
///
/// * Because `loop` and `if` naturally end with a braces body that define their
///   scope, they do not need to be followed by a `;`.
/// * All other statements must have their endings marked with a `;`. Multiple
///   `;` are allowed in a row, the same as having an empty block is allowed.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Statement {
  pub span: Span32,
  // Note(Lokathor): most statements have 0 attributes, and there are many
  // statemnts in a program, so we `Option<Box<_>>` these.
  pub attributes: Option<Box<Vec<Attribute>>>,
  pub kind: Box<StatementKind>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum StatementKind {
  StatementKindError,
  Let(StrID, Span32, Option<TypeName>, Option<Expr>),
  Assign(Expr, Expr),
  BinOpAssign(Expr, BinOpKind, Expr),
  IfElse(IfElseInfo),
  Loop(LoopInfo),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Expr {
  pub span: Span32,
  pub kind: Box<ExprKind>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ExprKind {
  ExprKindError,
  NumLit(StrID),
  StrLit(StrID),
  Ident(StrID),
  Bool(bool),
  StructLit(StrID, Span32, Vec<StructFieldInitKind>),
  Macro(StrID, Vec<Expr>),
  List(Vec<Expr>),
  Block(AstSatementBody),
  BinOp(Expr, BinOpKind, Expr),
  UnOp(Expr, UnOpKind),
  Loop(LoopInfo),
  IfElse(IfElseInfo),
  Break(Option<(StrID, Span32)>, Option<Expr>),
  Continue(Option<(StrID, Span32)>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BinOpKind {
  Call,
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
  Path,
  Eq,
  Ne,
  Lt,
  Le,
  Gt,
  Ge,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UnOpKind {
  /// `*x`
  Deref,
  /// `-x`
  Neg,
  /// `&x`
  Ref,
  /// `!x`
  Not,
  /// `return x`
  Return,
  /// ``` `x  ```
  Backtick,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LoopInfo {
  pub name: Option<(StrID, Span32)>,
  pub steps: AstSatementBody,
  /// `loop EXPR times { ... }`
  ///
  /// if this is declared then the loop runs "expr" times in a row without the
  /// programmer having to manually adjust a counter variable and perform checks
  /// and break.
  pub times: Option<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IfElseInfo {
  pub condition: Expr,
  pub if_body: AstSatementBody,
  pub else_body: Option<AstSatementBody>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum StructFieldInitKind {
  StructFieldInitKindError,
  Assign(StrID, Span32, Expr),
  Set(StrID, Span32),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AstConst {
  pub name: StrID,
  pub name_span: Span32,
  pub ty: TypeName,
  pub expr: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AstStatic {
  pub name: StrID,
  pub name_span: Span32,
  pub kind: StaticKind,
  pub ty: TypeName,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum StaticKind {
  Rom(Expr),
  Ram(Expr),
  MMIO,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AstStruct {
  pub name: StrID,
  pub name_span: StrID,
  pub fields: Vec<(StrID, Span32, TypeName)>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AstBitbag {
  pub name: StrID,
  pub name_span: StrID,
  pub fields: Vec<Option<(StrID, Span32, Expr)>>,
}
