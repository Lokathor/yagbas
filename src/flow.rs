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

/// How does control flow after a block?
///
/// As this phase, the condition is still a full AST [Expr] value. We break the
/// `Expr` into sub-steps and pick the condition flag to branch on later during
/// SSA creation.
#[derive(Debug, Clone)]
pub enum AstBlockFlow {
  /// Fall/jump to this block.
  Always(BlockID),
  /// Branch on the given expression
  Branch(S<Expr>, BlockID, BlockID),
  /// return the caller.
  Return,
}

/// One step in an AST block.
#[derive(Debug, Clone)]
pub enum AstBlockStep {
  /// Evaluate an expression.
  Expr(Expr),
  /// Call a function.
  ///
  /// * Functions dont currently return a value, so they are always their own
  ///   separate statement.
  /// * The ABI of all functions is currently that they pass in all registers
  ///   and all of memory, and then all inputs are clobbered by the call. A
  ///   better ABI might be implemented in the future.
  Call(StrID),
  /// An error of some kind.
  ///
  /// Likely causes:
  /// * the source AST had an error.
  /// * a break/continue target label was not found.
  AstBlockStepError,
}

/// Basic Blocks holding AST values.
#[derive(Debug, Clone)]
pub struct AstBlock {
  pub id: BlockID,
  pub steps: Vec<S<AstBlockStep>>,
  pub next: AstBlockFlow,
}
impl AstBlock {
  fn new() -> Self {
    Self { id: BlockID::new(), steps: Vec::new(), next: AstBlockFlow::Return }
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
    let mut current = blocks.last_mut().unwrap();
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
          Statement::Return => {
            current.next = AstBlockFlow::Return;
            if statement_iter.peek().is_some() {
              // TODO: unreachable code warning.
            }
            break 'statement_walk;
          }
          Statement::Break(str_id) => {
            let target = str_id.unwrap_or_default();
            let opt_break_target = loop_stack
              .iter()
              .rev()
              .find(|(label, _here, _after)| *label == target);
            match opt_break_target {
              None => {
                // todo: error
              }
              Some((_label, _here, after)) => {
                current.next = AstBlockFlow::Always(*after);
                if statement_iter.peek().is_some() {
                  // TODO: unreachable code warning.
                }
                break 'statement_walk;
              }
            }
          }
          Statement::Continue(str_id) => {
            let target = str_id.unwrap_or_default();
            let opt_break_target = loop_stack
              .iter()
              .rev()
              .find(|(label, _here, _after)| *label == target);
            match opt_break_target {
              None => {
                // todo: error
              }
              Some((_label, here, _after)) => {
                current.next = AstBlockFlow::Always(*here);
                if statement_iter.peek().is_some() {
                  // TODO: unreachable code warning.
                }
                break 'statement_walk;
              }
            }
          }
          Statement::IfElse(if_else) => {
            let mut if_block = AstBlock::new();
            let mut else_block = AstBlock::new();
            let mut after_block = AstBlock::new();
            current.next = AstBlockFlow::Branch(
              if_else.condition.clone(),
              if_block.id,
              else_block.id,
            );
            if_block.next = AstBlockFlow::Always(after_block.id);
            else_block.next = AstBlockFlow::Always(after_block.id);
            blocks.push(if_block);
            recursive_inner(if_else.if_body.as_slice(), blocks, loop_stack);
            blocks.push(else_block);
            recursive_inner(if_else.else_body.as_slice(), blocks, loop_stack);
            blocks.push(after_block);
            current = blocks.last_mut().unwrap();
          }
          Statement::Loop(loop_) => {
            let mut here_block = AstBlock::new();
            let here_block_id = here_block.id;
            let mut after_block = AstBlock::new();
            let after_block_id = after_block.id;
            current.next = AstBlockFlow::Always(here_block.id);
            here_block.next = AstBlockFlow::Always(here_block.id);
            blocks.push(here_block);
            loop_stack.push((
              loop_.opt_name.0.unwrap_or_default(),
              here_block_id,
              after_block_id,
            ));
            recursive_inner(loop_.body.as_slice(), blocks, loop_stack);
            loop_stack.pop();
            current = blocks.last_mut().unwrap();
          }
        }
      }
      // no more statements
    }
  }
  //
  let mut out = vec![AstBlock::new()];
  let mut label_stack = Vec::new();
  recursive_inner(statements, &mut out, &mut label_stack);
  out
}
