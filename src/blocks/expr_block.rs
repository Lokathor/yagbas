use super::*;

/// Block flow with [Expr] branch consitions.
#[derive(Debug, Clone, Display)]
pub enum ExprBlockFlow {
  /// Jump to this block.
  Always(BlockID),
  /// Branch on the given expression
  #[display("Branch({_0}, {_1}, {_2})")]
  Branch(S<Expr>, BlockID, BlockID),
  /// return the caller.
  Return,
}

#[derive(Debug, Clone, Default, Display)]
pub enum ExprBlockStep {
  /// Evaluate an expression.
  Expr(S<Expr>),
  /// Call a function.
  ///
  /// * Functions dont currently return a value, so they are always their own
  ///   separate statement.
  /// * The ABI of all functions is currently that they pass in all registers
  ///   and all of memory, and then all inputs are clobbered by the call. A
  ///   better ABI might be implemented in the future.
  #[display("{_0}()")]
  Call(StrID),
  /// An error of some kind.
  ///
  /// Likely causes:
  /// * the source AST had an error.
  /// * a break/continue target label was not found.
  #[default]
  ExprBlockStepError,
}

#[derive(Debug, Clone)]
pub struct ExprBlock {
  pub id: BlockID,
  pub steps: Vec<S<ExprBlockStep>>,
  pub next: ExprBlockFlow,
}
impl ExprBlock {
  #[inline]
  pub fn new() -> Self {
    Self { id: BlockID::new(), steps: Vec::new(), next: ExprBlockFlow::Return }
  }
}
impl Default for ExprBlock {
  #[inline]
  fn default() -> Self {
    Self::new()
  }
}

pub fn separate_ast_statements_into_blocks(
  statements: &[S<Statement>],
) -> Vec<ExprBlock> {
  /// * `statements` is the body of the function being broken up
  /// * `blocks` is the list of blocks we're working on
  /// * `loop_stack` is a list of loop labels and the id of their body and after
  ///   their body.
  fn recursive_inner(
    statements: &[S<Statement>], blocks: &mut Vec<ExprBlock>,
    loop_stack: &mut Vec<(StrID, BlockID, BlockID)>,
  ) {
    let mut current = blocks.last_mut().unwrap();
    let mut statement_iter = statements.iter().peekable();
    'statement_walk: loop {
      // note(Lokathor): we can't use `for` with this iterator because we need
      // to check if there's more statements after a break/continue/return and
      // emit a warning when that happens.
      if let Some(S(statement, span)) = statement_iter.next() {
        match statement {
          // these just carry forward
          Statement::Expr(expr) => {
            current.steps.push(S(ExprBlockStep::Expr(expr.clone()), *span));
          }
          Statement::Call(str_id) => {
            current.steps.push(S(ExprBlockStep::Call(*str_id), *span));
          }
          Statement::StatementError => {
            current.steps.push(S(ExprBlockStep::ExprBlockStepError, *span));
          }
          Statement::Return => {
            // new blocks default to Return as their control flow, but the
            // creator of the current block *might* have set something else, so
            // we have to be sure to set it back to Return.
            current.next = ExprBlockFlow::Return;
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
                current.next = ExprBlockFlow::Always(*after);
              }
            }
            if statement_iter.peek().is_some() {
              // TODO: unreachable code warning.
            }
            break 'statement_walk;
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
                current.next = ExprBlockFlow::Always(*here);
              }
            }
            if statement_iter.peek().is_some() {
              // TODO: unreachable code warning.
            }
            break 'statement_walk;
          }
          Statement::IfElse(if_else) => {
            let mut if_block = ExprBlock::new();
            let mut else_block = ExprBlock::new();
            let mut after_block = ExprBlock::new();
            after_block.next = replace(
              &mut current.next,
              ExprBlockFlow::Branch(
                if_else.condition.clone(),
                if_block.id,
                else_block.id,
              ),
            );
            if_block.next = ExprBlockFlow::Always(after_block.id);
            else_block.next = ExprBlockFlow::Always(after_block.id);
            blocks.push(if_block);
            recursive_inner(if_else.if_body.as_slice(), blocks, loop_stack);
            blocks.push(else_block);
            recursive_inner(if_else.else_body.as_slice(), blocks, loop_stack);
            blocks.push(after_block);
            current = blocks.last_mut().unwrap();
          }
          Statement::Loop(loop_) => {
            let mut here_block = ExprBlock::new();
            let here_block_id = here_block.id;
            let mut after_block = ExprBlock::new();
            let after_block_id = after_block.id;
            after_block.next =
              replace(&mut current.next, ExprBlockFlow::Always(here_block.id));
            here_block.next = ExprBlockFlow::Always(here_block.id);
            blocks.push(here_block);
            loop_stack.push((
              loop_.opt_name.0.unwrap_or_default(),
              here_block_id,
              after_block_id,
            ));
            recursive_inner(loop_.body.as_slice(), blocks, loop_stack);
            loop_stack.pop();
            blocks.push(after_block);
            current = blocks.last_mut().unwrap();
          }
        }
      } else {
        break 'statement_walk;
      }
    }
  }
  //
  let mut out = vec![ExprBlock::new()];
  let mut label_stack = Vec::new();
  recursive_inner(statements, &mut out, &mut label_stack);
  out
}
