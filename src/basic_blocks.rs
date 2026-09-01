//! Module for Basic Block stuff.

use crate::cst::{
  Cst,
  CstKind::{self, ItemFunction, StmtEmpty, StmtExpression, StmtLet},
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
  #[allow(clippy::ptr_arg)]
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
