use std::cell::Cell;

use crate::tokenizer::Token;

#[derive(Debug, Clone)]
pub struct SyntaxTree {
  pub kind: SyntaxTreeKind,
  pub elements: Vec<TreeElement>,
}

#[derive(Debug, Clone, Copy)]
pub enum SyntaxTreeKind {
  TokenTreeError,
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

#[derive(Debug, Clone)]
enum ParserEvent {
  Open { kind: SyntaxTreeKind },
  Close,
  ADvance,
}

#[derive(Debug, Clone)]
struct OpenMark {
  index: usize,
}

#[derive(Debug, Clone)]
pub struct Parser {
  tokens: Vec<Token>,
  position: usize,
  fuel: Cell<u32>,
  events: Vec<ParserEvent>,
}
