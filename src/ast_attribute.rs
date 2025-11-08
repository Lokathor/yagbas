use super::*;

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub enum AstAttribute {
  #[default]
  AttributeError,
  /// like `#[hram]`
  Ident(StrID, Span32),
  /// like `#[game_revision = 2]`
  Assignment(StrID, Span32, Expr),
  /// like `#[location($FF00)]`
  Call(StrID, Span32, Vec<AstAttribute>),
}
