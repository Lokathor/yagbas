use super::*;

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct Statement {
  span: Span32,
  attribues: Vec<AstAttribute>,
  /// use `None` when there's an error, instead of an explicit error variant.
  kind: Option<StatementKind>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum StatementKind {
  /// `let varname: vartype;`
  Let(StrID, Option<StrID>),

  /// `let varname: vartype = expr;`
  ///
  /// The same as `Let` followed by an `Expr(Assign)`, but has a tag of its own
  /// so that the parser can return just one statement per `;` seperator. This
  /// should be broken up soon after the initial parsing.
  LetAssign(StrID, Option<StrID>, Expr),

  /// Any expression on its own can be a statement.
  ///
  /// Usually this will be an assignment, call, if-else, or loop.
  Expr(Expr),
}
