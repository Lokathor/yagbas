use super::*;

/// One single line of code in a body of code.
///
/// There's one `Vec<Statement>` per body of code in the AST, so we should be
/// mindful about the size of this type.
#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct Statement {
  span: Span32,
  /// most statements have 0 attributes, so we Option this.
  attribues: Option<Box<Vec<AstAttribute>>>,
  kind: Box<StatementKind>,
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
