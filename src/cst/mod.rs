use crate::tokenizer::Token;

pub mod operators;
pub mod parser_actions;
pub mod parser_core;

/// Concrete Syntax Tree
///
/// Use the `pretty_debug` method if you need to print debug info nicely.
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Cst {
  pub kind: CstKind,
  pub elements: Vec<CstElem>,
}
impl Cst {
  /// pretty-print the debug info of this Cst into a String.
  pub fn pretty_debug(&self) -> String {
    let mut buffer = String::new();
    self.pretty_debug_rec(0, &mut buffer);
    buffer
  }
  fn pretty_debug_rec(&self, indents: usize, buffer: &mut String) {
    use core::fmt::Write;
    for _ in 0..indents {
      write!(buffer, " ").ok();
    }
    writeln!(buffer, "{:?} {{", self.kind).ok();
    for element in &self.elements {
      match element {
        CstElem::Token(Token { kind, span }) => {
          for _ in 0..(indents + 2) {
            write!(buffer, " ").ok();
          }
          writeln!(buffer, "{kind:?} @({span:?})").ok();
        }
        CstElem::Tree(cst) => {
          cst.pretty_debug_rec(indents + 2, buffer);
        }
      }
    }
    for _ in 0..indents {
      write!(buffer, " ").ok();
    }
    writeln!(buffer, "}}").ok();
  }
}

/// I have no idea what the correct set of tags is here!
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CstKind {
  ErrNoTreeKindSet,
  ErrGeneric,
  ErrExpectedValueExpression,
  ErrExpectedTypeExpression,
  ErrNeedsParensToDisambiguate,
  //
  ValExpr,
  TypeExpr,
  AtomicValue,
  ParenGroup,
  InfixOperator,
  PrefixOperator,
  PostfixOperator,
  FnCallArgument,
  ArgumentList,
}

/// A single element within a [Cst].
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CstElem {
  Token(Token),
  Tree(Cst),
}
