use super::*;
use core::{
  num::NonZeroUsize,
  sync::atomic::{AtomicUsize, Ordering},
};
use str_id::StrID;

static NEXT_BLOCK_ID: AtomicUsize = AtomicUsize::new(1);

/// This is a newtype over a [NonZeroUsize].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct BlockID(NonZeroUsize);
impl BlockID {
  /// Makes a new ID.
  /// ## Failure
  /// If the next ID would be 0, this fails.
  #[inline]
  pub fn try_new() -> Option<Self> {
    NonZeroUsize::new(NEXT_BLOCK_ID.fetch_add(1, Ordering::Relaxed)).map(Self)
  }
  /// `BlockID::try_new().expect("...")`
  #[inline]
  #[track_caller]
  pub fn new() -> Self {
    Self::try_new().expect("exhausted the available BlockID values!")
  }

  /// Unwraps the value into a raw `usize`.
  #[inline]
  #[must_use]
  pub const fn as_usize(self) -> usize {
    self.0.get()
  }
}
impl Default for BlockID {
  #[inline]
  fn default() -> Self {
    Self::new()
  }
}

#[derive(Debug, Clone)]
pub enum BlockFlow {
  Always(BlockID),
  BranchCarry(BlockID, BlockID),
  BranchZero(BlockID, BlockID),
  Return,
}

#[derive(Debug, Clone)]
pub enum AstBlockStep {
  Expr(Expr),
  Call(StrID),
  StatementError,
}

#[derive(Debug, Clone)]
pub struct AstBlock {
  pub id: BlockID,
  pub steps: Vec<S<AstBlockStep>>,
  pub next: BlockFlow,
}
impl AstBlock {
  fn new() -> Self {
    Self { id: BlockID::new(), steps: Vec::new(), next: BlockFlow::Return }
  }
}

pub fn separate_ast_statements_into_blocks(
  statements: &[S<Statement>],
) -> Vec<AstBlock> {
  /// * `statements` is the body of the function being broken up
  /// * `blocks` is the list of blocks we're working on
  /// * `loop_stack` is a list of loop labels and the id of their body and after
  ///   their body.
  fn recursive_inner(
    statements: &[S<Statement>], blocks: &mut Vec<AstBlock>,
    loop_stack: &mut Vec<(StrID, BlockID, BlockID)>,
  ) {
    let mut current = AstBlock::new();
    let mut statement_iter = statements.iter().peekable();
    'statement_walk: loop {
      if let Some(S(statement, span)) = statement_iter.next() {
        match statement {
          // these just carry forward
          Statement::Expr(expr) => {
            current.steps.push(S(AstBlockStep::Expr(expr.clone()), *span))
          }
          Statement::Call(str_id) => {
            current.steps.push(S(AstBlockStep::Call(*str_id), *span))
          }
          Statement::StatementError => {
            current.steps.push(S(AstBlockStep::StatementError, *span))
          }
          // this ends a block's flow, and code shouldn't be after an
          // unconditional return.
          Statement::Return => {
            current.next = BlockFlow::Return;
            blocks.push(current);
            if statement_iter.peek().is_some() {
              // TODO: emit a warning/error about unreachable code when `return`
              // is followed by other statements in a code body.
            }
            break 'statement_walk;
          }
          Statement::IfElse(if_else) => todo!(),
          Statement::Loop(loop_) => todo!(),
          Statement::Break(str_id) => todo!(),
          Statement::Continue(str_id) => todo!(),
        }
      }
      // no more statements
    }
  }
  //
  let mut out = Vec::new();
  let mut label_stack = Vec::new();
  recursive_inner(statements, &mut out, &mut label_stack);
  out
}
