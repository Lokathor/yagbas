use std::cell::Cell;

use crate::tokenizer::Token;

#[derive(Debug, Clone)]
pub struct SyntaxTree {
  pub kind: SyntaxTreeKind,
  pub elements: Vec<TreeElement>,
}

#[derive(Debug, Clone, Copy)]
pub enum SyntaxTreeKind {
  SyntaxTreeKindError,
  Module,
  Bitbag,
  Structure,
  Static,
  Constant,
  Function,
  // TODO: we need all sorts of syntax tree kinds. the exact list will be
  // discovered as we build out the parser itself.
}

#[derive(Debug, Clone)]
pub enum TreeElement {
  Token(Token),
  Tree(SyntaxTree),
}
