use super::*;

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
