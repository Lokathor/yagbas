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
  pub attributes: Vec<Expr>,
  pub name: StrID,
  pub name_span: Span32,
  pub kind: AstItemKind,
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeName {
  pub span: Span32,
  pub kind: TypeNameKind,
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeNameKind {
  Ident(StrID),
  ArrayNumLit(Box<TypeName>, StrID, Span32),
  ArrayConstName(Box<TypeName>, StrID, Span32),
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
  pub attributes: Vec<Expr>,
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
  pub attributes: Vec<Expr>,
  pub name: StrID,
  pub name_span: Span32,
  pub ty: TypeName,
}

/// Gives a name to a constant expression.
///
/// ```yag
/// const NAME_HERE: Ty = Expression;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AstConst {
  pub ty: TypeName,
  pub expr: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AstStatic {
  pub ty: TypeName,
  pub kind: AstStaticKind,
}

/// Static data within the program.
///
/// ```yag
/// static memory_kind NAME_HERE: Ty = InitializationExpression;
/// ```
///
/// * `static rom` is immutable data, such as tile or tilemap data, with an expression encoded into the rom.
/// * `static ram` is plain mutable data, such as a current position. It has an expression that is used to initialize the ram before `main` begins.
/// * `static mmio` is volatile mutable data, for memory-mapped IO. It does not have an initialization expression.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AstStaticKind {
  Rom(Expr),
  Ram(Expr),
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
  pub return_info: Option<(StrID, Span32)>,
  pub body: StatementBody,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AstFunctionArg {
  pub span: Span32,
  pub attributes: Vec<Expr>,
  pub name: StrID,
  pub name_span: Span32,
  pub ty: TypeName,
}

/// One single line of code in a body of code.
///
/// There's one `Vec<Statement>` per body of code in the AST, so we should be
/// mindful about the size of this type.
#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct Statement {
  pub span: Span32,
  /// most statements have 0 attributes, and there are many statemnts in a
  /// program, so we Option this.
  pub attribues: Option<Box<Vec<Expr>>>,
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

  /// `let varname: vartype`
  Let(StrID, Option<TypeName>),

  /// `let varname: vartype = expr;`
  LetAssign(StrID, Option<TypeName>, Expr),

  /// `lhs = rhs`
  Assign(Expr, Expr),

  /// `lhs += rhs`
  AddAssign(Expr, Expr),

  /// `lhs -= rhs`
  SubAssign(Expr, Expr),

  /// `lhs *= rhs`
  MulAssign(Expr, Expr),

  /// `lhs /= rhs`
  DivAssign(Expr, Expr),

  /// `lhs %= rhs`
  ModAssign(Expr, Expr),

  /// `lhs <<= rhs`
  ShiftLeftAssign(Expr, Expr),

  /// `lhs >>= rhs`
  ShiftRightAssign(Expr, Expr),

  /// `lhs &= rhs`
  BitAndAssign(Expr, Expr),

  /// `lhs |= rhs`
  BitOrAssign(Expr, Expr),

  /// `lhs ^= rhs`
  BitXorAssign(Expr, Expr),
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct Expr {
  pub span: Span32,
  pub kind: Box<ExprKind>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub enum ExprKind {
  #[default]
  ExprKindError,

  NumLit(StrID),
  Ident(StrID),
  Bool(bool),

  /// `[ ... , ... , ... ]`
  ///
  /// square brackets around sub-expressions that forms a series of elements.
  List(Vec<Expr>),

  /// `{ ... ; ... ; ... }`
  ///
  /// braces around a series of statements.
  Block(StatementBody),

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
  /// `!x`
  Not,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExprBinOp {
  pub lhs: Expr,
  pub rhs: Expr,
  pub kind: BinOpKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
  Eq,
  Ne,
  Lt,
  Le,
  Gt,
  Ge,
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
  pub args: Vec<Expr>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct StatementBody {
  pub body: Vec<Statement>,
  pub trailing_semicolon: bool,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct ExprIfElse {
  pub condition: Expr,
  pub if_: StatementBody,
  pub else_: Option<StatementBody>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct ExprLoop {
  pub name: Option<(StrID, Span32)>,
  pub steps: StatementBody,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct ExprLoopTimes {
  pub name: Option<(StrID, Span32)>,
  pub times: StrID,
  pub times_span: Span32,
  pub steps: StatementBody,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct ExprBreak {
  pub target: Option<(StrID, Span32)>,
  pub value: Option<Expr>,
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub struct ExprContinue {
  pub target: Option<(StrID, Span32)>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct ExprReturn {
  pub value: Option<Expr>,
}

pub fn items_of<'src>(
  trees: &'src [(TokenTree, Span32)], yag_state: YagParserState,
) -> (Vec<AstItem>, Vec<Rich<'src, TokenTree, Span32>>) {
  let eoi: Span32 = match trees.last() {
    Some(s) => s.1,
    None => return (Vec::new(), Vec::new()),
  };
  let mut simple_state = SimpleState(yag_state);

  let item_parser = item_p().repeated().collect::<Vec<_>>();

  let (opt_out, errors): (Option<Vec<AstItem>>, Vec<_>) = item_parser
    .parse_with_state(
      Input::map(trees, eoi, |(tk, span)| (tk, span)),
      &mut simple_state,
    )
    .into_output_errors();

  (opt_out.unwrap_or_default(), errors)
}
