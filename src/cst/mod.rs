#![allow(unused_imports)]

use crate::tokenizer::Token;
use crate::tokenizer::TokenKind;

pub mod actions;
pub mod operators;
pub mod parser;

/// Concrete Syntax Tree
///
/// Use the `pretty_debug` method if you need to print debug info nicely.
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Cst {
  pub kind: CstKind,
  pub elements: Vec<CstElem>,
}
impl core::fmt::Display for Cst {
  /// Better way to look at the tree than Debug provides.
  ///
  /// * use the alternate flag to enable displaying of whitespace and comment tokens, as well as commentary syntax trees. Otherwise they are skipped from the output.
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    return fmt_rec(self, f, 0);

    fn fmt_rec(
      s: &Cst, f: &mut core::fmt::Formatter<'_>, indents: usize,
    ) -> core::fmt::Result {
      use core::fmt::Write;
      for _ in 0..indents {
        write!(f, " ")?;
      }
      writeln!(f, "{:?} {{", s.kind)?;
      for element in &s.elements {
        match element {
          CstElem::Token(Token { kind, position }) => {
            if !f.alternate()
              && (*kind == TokenKind::Comment || *kind == TokenKind::Whitespace)
            {
              continue;
            }
            for _ in 0..(indents + 2) {
              write!(f, " ")?;
            }
            writeln!(f, "{kind:?} @({position:?})")?;
          }
          CstElem::Tree(cst) => {
            fmt_rec(cst, f, indents + 2)?;
          }
        }
      }
      for _ in 0..indents {
        write!(f, " ")?;
      }
      writeln!(f, "}}")
    }
  }
}

/// I have no idea what the correct set of tags is here!
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CstKind {
  ErrNoTreeKindSet,
  ErrGeneric,
  ErrExpectedItemKeyword,
  ErrExpectedValueExpression,
  ErrExpectedTypeExpression,
  ErrNeedsParensToDisambiguate,
  ErrTodo,
  ErrExpected(TokenKind),
  ErrExpectedBody,
  ErrExpectedIfCondition,
  //
  Module,
  ValExpr,
  TypeExpr,
  AtomicValue,
  ParenGroup,
  InfixOperator,
  PrefixOperator,
  PostfixOperator,
  FnCallArgument,
  ArgumentList,
  StmtLet,
  StmtValExpr,
  StmtItem,
  StmtLoop,
  StmtExpression,
  StmtIf,
  StmtFor,
  Function,
  ReturnType,
  Body,
  StmtEmpty,
  IfCondition,
}

/// A single element within a [Cst].
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CstElem {
  Token(Token),
  Tree(Cst),
}
