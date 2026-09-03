use core::ops::Range;
use str_id::StrId;
use tinyvec::ArrayVec;

use crate::ast::{
  AstBody, AstExprVal, AstExprValKind, AstFunction, AstStatementKind,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BlockId(usize);

#[derive(Debug, Clone)]
pub struct TacBlockStep {
  pub span: Range<usize>,
  pub kind: TacBlockStepKind,
}
#[derive(Debug, Clone, Copy)]
pub enum TacBlockStepKind {
  ErrTacBlockStepKind,
  /// `*dst = src;`
  DerefStore(StrId, StrId),
  /// `dst = *src;`
  DerefLoad(StrId, StrId),
  /// `dst = a == b;`
  CmpEq(StrId, StrId, StrId),
  // TODO
}

#[derive(Debug, Clone, Copy)]
pub enum TacBlockTerminator {
  /// return to caller
  Return,
  /// jump to the block given
  JumpTo(BlockId),
  /// `if cond { jump a } else { jump b }`
  If(StrId, BlockId, BlockId),
}

#[derive(Debug, Clone)]
pub struct TacBlock {
  pub id: BlockId,
  pub steps: Vec<TacBlockStep>,
  pub terminator: TacBlockTerminator,
}

#[derive(Debug, Clone, Default)]
struct TacBuilderContext {
  blocks: Vec<TacBlock>,
  next_block_id: usize,
  next_temp_id: usize,
  label_stack: Vec<(StrId, BlockId)>,
}
impl TacBuilderContext {
  fn make_block_id(&mut self) -> BlockId {
    let out = BlockId(self.next_block_id);
    self.next_block_id += 1;
    out
  }
  fn make_temp_var(&mut self) -> StrId {
    use core::fmt::Write;
    let id = self.next_temp_id;
    self.next_temp_id += 1;
    let mut buf: ArrayVec<[u8; 32]> = ArrayVec::default();
    write!(buf, "#{id}").unwrap();
    StrId::from(str::from_utf8(&buf).unwrap())
  }

  #[allow(clippy::match_single_binding)]
  fn handle_body(&mut self, body: &AstBody) {
    for stmt in body.statements.iter() {
      match &stmt.kind {
        AstStatementKind::Item(_) => continue,
        AstStatementKind::ErrAstStatementKind => {
          self.blocks.last_mut().unwrap().steps.push(TacBlockStep {
            span: stmt.span.clone(),
            kind: TacBlockStepKind::ErrTacBlockStepKind,
          });
        }
        //
        AstStatementKind::Let(ast_let) => match &ast_let.pattern.kind {
          _other => {
            dbg!(&_other);
            self.blocks.last_mut().unwrap().steps.push(TacBlockStep {
              span: stmt.span.clone(),
              kind: TacBlockStepKind::ErrTacBlockStepKind,
            });
          }
        },
        AstStatementKind::Expression(ast_expr_val) => {
          self.handle_expression(ast_expr_val);
        }
      }
    }
  }

  /// returns the name of the variable that the expression went into.
  fn handle_expression(&mut self, ast_expr_val: &AstExprVal) -> Option<StrId> {
    match &ast_expr_val.kind {
      AstExprValKind::ErrAstValExprKind => {
        self.blocks.last_mut().unwrap().steps.push(TacBlockStep {
          span: ast_expr_val.span.clone(),
          kind: TacBlockStepKind::ErrTacBlockStepKind,
        });
      }
      AstExprValKind::Assign(dst, src) => match &dst.kind {
        AstExprValKind::Dereference(p) => match &p.kind {
          AstExprValKind::Identifier(i) => match &src.kind {
            AstExprValKind::Identifier(i2) => {
              self.blocks.last_mut().unwrap().steps.push(TacBlockStep {
                span: ast_expr_val.span.clone(),
                kind: TacBlockStepKind::DerefStore(*i, *i2),
              });
              return Some(*i);
            }
            AstExprValKind::LiteralNumber(i2) => {
              self.blocks.last_mut().unwrap().steps.push(TacBlockStep {
                span: ast_expr_val.span.clone(),
                kind: TacBlockStepKind::DerefStore(*i, *i2),
              });
              return Some(*i);
            }
            _other => {
              dbg!(&_other);
              self.blocks.last_mut().unwrap().steps.push(TacBlockStep {
                span: ast_expr_val.span.clone(),
                kind: TacBlockStepKind::ErrTacBlockStepKind,
              });
            }
          },
          _other => {
            dbg!(&_other);
            self.blocks.last_mut().unwrap().steps.push(TacBlockStep {
              span: ast_expr_val.span.clone(),
              kind: TacBlockStepKind::ErrTacBlockStepKind,
            });
          }
        },
        _other => {
          dbg!(&_other);
          self.blocks.last_mut().unwrap().steps.push(TacBlockStep {
            span: ast_expr_val.span.clone(),
            kind: TacBlockStepKind::ErrTacBlockStepKind,
          });
        }
      },
      AstExprValKind::Loop(body) => {
        let loop_id = self.make_block_id();
        let after_id = self.make_block_id();
        let after_terminator = core::mem::replace(
          &mut self.blocks.last_mut().unwrap().terminator,
          TacBlockTerminator::JumpTo(loop_id),
        );
        let loop_block = TacBlock {
          id: loop_id,
          steps: Vec::new(),
          terminator: TacBlockTerminator::JumpTo(loop_id),
        };
        self.blocks.push(loop_block);
        self.label_stack.push((StrId::from(""), after_id));
        self.handle_body(body);
        self.label_stack.pop();
        let after_block = TacBlock {
          id: after_id,
          steps: Vec::new(),
          terminator: after_terminator,
        };
        self.blocks.push(after_block);
      }
      AstExprValKind::If(cond, body) => {
        let Some(cond_var) = self.handle_expression(cond) else {
          self.blocks.last_mut().unwrap().steps.push(TacBlockStep {
            span: ast_expr_val.span.clone(),
            kind: TacBlockStepKind::ErrTacBlockStepKind,
          });
          return None;
        };
        let if_id = self.make_block_id();
        let after_id = self.make_block_id();
        let after_terminator = core::mem::replace(
          &mut self.blocks.last_mut().unwrap().terminator,
          TacBlockTerminator::If(cond_var, if_id, after_id),
        );
        let if_block = TacBlock {
          id: if_id,
          steps: Vec::new(),
          terminator: TacBlockTerminator::JumpTo(after_id),
        };
        self.blocks.push(if_block);
        self.handle_body(body);
        let after_block = TacBlock {
          id: after_id,
          steps: Vec::new(),
          terminator: after_terminator,
        };
        self.blocks.push(after_block);
      }
      AstExprValKind::CmpEq(lhs_xpr, rhs_xpr) => {
        match [self.handle_expression(lhs_xpr), self.handle_expression(rhs_xpr)]
        {
          [Some(lhs), Some(rhs)] => {
            let tmp = self.make_temp_var();
            self.blocks.last_mut().unwrap().steps.push(TacBlockStep {
              span: ast_expr_val.span.clone(),
              kind: TacBlockStepKind::CmpEq(tmp, lhs, rhs),
            });
            return Some(tmp);
          }
          _other => {
            self.blocks.last_mut().unwrap().steps.push(TacBlockStep {
              span: ast_expr_val.span.clone(),
              kind: TacBlockStepKind::ErrTacBlockStepKind,
            });
          }
        }
      }
      AstExprValKind::Dereference(inner_xpr) => {
        if let Some(inner) = self.handle_expression(inner_xpr) {
          let tmp = self.make_temp_var();
          self.blocks.last_mut().unwrap().steps.push(TacBlockStep {
            span: ast_expr_val.span.clone(),
            kind: TacBlockStepKind::DerefLoad(tmp, inner),
          });
          return Some(tmp);
        } else {
          self.blocks.last_mut().unwrap().steps.push(TacBlockStep {
            span: inner_xpr.span.clone(),
            kind: TacBlockStepKind::ErrTacBlockStepKind,
          });
        }
      }
      AstExprValKind::Identifier(i) => return Some(*i),
      AstExprValKind::Break => {
        dbg!("hello");
        let target_id = StrId::default();
        let Some((_, block_id)) =
          self.label_stack.iter().rev().find(|(l, _)| *l == target_id)
        else {
          dbg!("label not found");
          self.blocks.last_mut().unwrap().steps.push(TacBlockStep {
            span: ast_expr_val.span.clone(),
            kind: TacBlockStepKind::ErrTacBlockStepKind,
          });
          return None;
        };
        self.blocks.last_mut().unwrap().terminator =
          TacBlockTerminator::JumpTo(*block_id);
      }
      _other => {
        dbg!(&_other);
        self.blocks.last_mut().unwrap().steps.push(TacBlockStep {
          span: ast_expr_val.span.clone(),
          kind: TacBlockStepKind::ErrTacBlockStepKind,
        });
      }
    }
    None
  }
}

pub fn tac_blocks_of_function(f: &AstFunction) -> Vec<TacBlock> {
  let mut ctx = TacBuilderContext::default();
  let id = ctx.make_block_id();
  ctx.blocks.push(TacBlock {
    id,
    steps: Vec::new(),
    terminator: TacBlockTerminator::Return,
  });
  ctx.handle_body(&f.body);
  ctx.blocks
}
