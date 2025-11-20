use super::*;

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub enum Statement {
  #[default]
  StatementError,

  /// `let varname: vartype;`
  Let(Vec<AstAttribute>, StrID, Option<StrID>),

  /// `let varname: vartype = expr;`
  ///
  /// The same as `Let` followed by an `Expr(Assign)`, but has a tag of its own
  /// so that the parser can return just one statement per `;` seperator. This
  /// should be broken up soon after the initial parsing.
  LetAssign(Vec<AstAttribute>, StrID, Option<StrID>, Expr),

  Expr(Vec<AstAttribute>, Expr),
}
