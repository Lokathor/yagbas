//! Module for Basic Block stuff.

use crate::cst::{
  Cst,
  CstKind::{self, ItemFunction, StmtEmpty, StmtExpression, StmtLet, StmtLoop},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BlockId(usize);

#[derive(Debug, Clone)]
pub struct BasicBlock {
  pub id: BlockId,
  pub statements: Vec<BasicBlockStatement>,
  pub terminator: BasicBlockTerminator,
}
impl BasicBlock {
  fn new(id: BlockId) -> Self {
    Self {
      id,
      statements: Vec::new(),
      terminator: BasicBlockTerminator::Return(None),
    }
  }
}

#[derive(Debug, Clone)]
pub enum BasicBlockStatement {
  /// Introduce a new named variable and assign it to the expression given.
  ///
  /// TODO: let statements should just introduce variables, and assigning the
  /// value to the variable should be broken into a separate statement.
  Let { pattern: Option<Cst>, expression: Option<Cst> },
  /// TODO: remove this and make it more precise
  OtherExpression(Cst),
}

#[derive(Debug, Clone)]
pub enum BasicBlockTerminator {
  /// Return control flow to the caller, optionally including an expression.
  Return(Option<Cst>),
  AlwaysJump(BlockId),
  ConditionJump(Cst, BlockId, BlockId),
}

pub fn basic_blocks_of(cst: &Cst) -> Vec<BasicBlock> {
  let mut blocks = Vec::new();
  if cst.kind != ItemFunction {
    return blocks;
  }
  let Some(body) = cst.sub_trees().find(|cst| cst.kind == CstKind::Body) else {
    return blocks;
  };
  let mut next_id = 0_usize;
  blocks.push(BasicBlock::new(BlockId(next_id)));
  next_id += 1;
  rec_basic_blocks_of(body, &mut blocks, &mut next_id);
  return blocks;

  #[allow(unused)]
  fn rec_basic_blocks_of(
    body: &Cst, blocks: &mut Vec<BasicBlock>, next_id: &mut usize,
  ) {
    for stmt_tree in body.sub_trees() {
      match stmt_tree.kind {
        StmtEmpty => continue,
        StmtLet => {
          let pattern = stmt_tree
            .sub_trees()
            .find(|st| st.kind == CstKind::Pattern)
            .cloned();
          let expression = stmt_tree
            .sub_trees()
            .find(|st| st.kind == CstKind::ValExpr)
            .cloned();
          blocks
            .last_mut()
            .unwrap()
            .statements
            .push(BasicBlockStatement::Let { pattern, expression })
        }
        StmtLoop => {
          let loop_body_id = BlockId(*next_id);
          *next_id += 1;
          let loop_after_id = BlockId(*next_id);
          *next_id += 1;
          let Some(loop_body) =
            stmt_tree.sub_trees().find(|cst| cst.kind == CstKind::Body)
          else {
            // do we need to log an error here?
            continue;
          };
          let mut loop_body_block = BasicBlock::new(loop_body_id);
          let mut loop_after_block = BasicBlock::new(loop_after_id);
          blocks.last_mut().unwrap().terminator =
            BasicBlockTerminator::AlwaysJump(loop_body_block.id);
          blocks.push(loop_body_block);
          // oops!! no labels went into here so the recursive call can't tell
          // where to break to if it sees a break expression.
          rec_basic_blocks_of(loop_body, blocks, next_id);
        }
        StmtExpression => blocks
          .last_mut()
          .unwrap()
          .statements
          .push(BasicBlockStatement::OtherExpression(stmt_tree.clone())),
        _other => {
          println!("unahndled: {_other:?}");
          continue;
        }
      }
    }
  }
}
