use chumsky::span::SimpleSpan;
use str_id::StrID;
use yagbas_srcfiletypes::FileID;

/// Generic typed value paired with a `SimpleSpan`
#[derive(Debug, Clone)]
pub struct S<T>(pub T, pub SimpleSpan);

/// The Abstract Syntax Tree for the whole program.
///
/// While each individual item is from a particular source file, the AST
/// combines all the items across all the files.
#[derive(Debug, Clone)]
pub struct Ast {
  pub items: Vec<S<Item>>,
}

/// An item is basically "a top level definition within a source file".
#[derive(Debug, Clone)]
pub enum Item {
  BitStruct(BitStruct),
  Const(Const),
  Func(Func),
  Static(Static),
  Struct(Struct),
  ItemError,
}

/// Bitpacked structured data.
///
/// An 8-bit value, where each bit position can have a field name.
#[derive(Debug, Clone)]
pub struct BitStruct {
  pub file_id: FileID,
  pub name: S<StrID>,
  /// `field_name: bit` list
  pub fields: Vec<(S<StrID>, S<StrID>)>,
}

/// Constant definition
///
/// This associates a name to a particular constant expression. A const is only
/// used during compile time, it doesn't add data in the compiled binary.
#[derive(Debug, Clone)]
pub struct Const {
  pub file_id: FileID,
  pub name: S<StrID>,
  pub expr: S<Expr>,
}

/// Function definition
///
/// Defines a body of code that can be executed.
#[derive(Debug, Clone)]
pub struct Func {
  pub file_id: FileID,
  pub name: S<StrID>,
  pub body: Vec<S<Statement>>,
}

/// Static definition
///
/// This is a block of data that's either in ROM or RAM. Generally this doesn't
/// represent code, but instead some other form of data that the program uses,
/// such as tile data.
#[derive(Debug, Clone)]
pub struct Static {
  pub file_id: FileID,
  pub name: S<StrID>,
  pub expr: S<Expr>,
}

/// Structured data.
#[derive(Debug, Clone)]
pub struct Struct {
  pub file_id: FileID,
  pub name: S<StrID>,
  /// `field_name: field_type` list
  pub fields: Vec<(S<StrID>, S<StrID>)>,
}

/// Any of the things that can go in a body of code.
#[derive(Debug, Clone)]
pub enum Statement {
  Expr(Expr),
  IfElse(Box<IfElse>),
  Loop(Box<Loop>),
  Break(Option<StrID>),
  Continue(Option<StrID>),
  Call(StrID),
  Return,
  StatementError,
}

/// An expression is a thing that can be made of sub-expressions.
///
/// Within the AST, expressions are untyped. Applying types (and generating type
/// errors) happens after the basic AST is parsed.
#[derive(Debug, Clone)]
pub enum Expr {
  /// `123`, `$FF`, `%10_01_00_00`, etc
  NumberLiteral(StrID),

  /// `main`, `memcpy`, etc
  Ident(StrID),

  /// `a`, `hl`, etc
  Register(Register),

  /// `LcdCtrl { display_on, bg_on }`
  BitStructLiteral(Box<Vec<S<StrID>>>),

  /// `Position { x: 15, y: 20 }`
  StructLiteral(Box<Vec<(S<StrID>, S<Self>)>>),

  /// `{ expr0, expr1, ..., exprN }`
  List(Box<Vec<S<Expr>>>),

  /// `[hl]`
  Deref(Box<Vec<S<Expr>>>),

  /// `macro_name!( expr0, expr1, ... exprN )`
  MacroUse(Box<Vec<S<Expr>>>),

  /// `array.0`, `b.LcdCtrl.bg_on`, etc
  Dot(Box<[S<Self>; 2]>),

  /// `-12`
  Neg(Box<S<Self>>),

  /// `a = 12`
  Assign(Box<[S<Self>; 2]>),

  /// `12 + 4`
  Add(Box<[S<Self>; 2]>),

  /// `12 - 4`
  Sub(Box<[S<Self>; 2]>),

  /// `12 * 4`
  Mul(Box<[S<Self>; 2]>),

  /// `12 / 4`
  Div(Box<[S<Self>; 2]>),

  /// `12 & 4`
  BitAnd(Box<[S<Self>; 2]>),

  /// `12 | 4`
  BitOr(Box<[S<Self>; 2]>),

  /// `12 ^ 4`
  BitXor(Box<[S<Self>; 2]>),

  /// Any error during expression processing
  ExprError,
}

/// Branching construct.
#[derive(Debug, Clone)]
pub struct IfElse {
  pub condition: S<Expr>,
  pub if_body: Vec<S<Statement>>,
  pub else_body: Vec<S<Statement>>,
}

/// Repeating code construct.
#[derive(Debug, Clone)]
pub struct Loop {
  pub name: Option<S<StrID>>,
  pub body: Vec<S<Statement>>,
}

/// The registers that can be named anywhere in any expression.
///
/// This type isn't strictly necessary, we could use [StrID] in all places where
/// there's a Register value, but it's a little faster to sort out registers
/// from identifiers ahead of time.
#[derive(Debug, Clone, Copy)]
pub enum Register {
  A,
  B,
  C,
  D,
  E,
  H,
  L,
  AF,
  BC,
  DE,
  HL,
  SP,
}
