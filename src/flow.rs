use super::*;
use core::{
  num::NonZeroUsize,
  sync::atomic::{AtomicUsize, Ordering},
};
use str_id::StrID;
use std::collections::HashMap;

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
  /// Jump to this block.
  Always(BlockID),
  /// Branch on the given expression
  Branch(S<Expr>, BlockID, BlockID),
  /// return the caller.
  Return,
}

/// One step in an AST block.
#[derive(Debug, Clone, Default)]
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
  #[default]
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
  #[inline]
  pub fn new() -> Self {
    Self { id: BlockID::new(), steps: Vec::new(), next: AstBlockFlow::Return }
  }
}
impl Default for AstBlock {
  #[inline]
  fn default() -> Self {
    Self::new()
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
      // note(Lokathor): we can't use `for` with this iterator because we need
      // to check if there's more statements after a break/continue/return and
      // emit a warning when that happens.
      if let Some(S(statement, span)) = statement_iter.next() {
        match statement {
          // these just carry forward
          Statement::Expr(expr) => {
            current.steps.push(S(AstBlockStep::Expr(expr.clone()), *span));
          }
          Statement::Call(str_id) => {
            current.steps.push(S(AstBlockStep::Call(*str_id), *span));
          }
          Statement::StatementError => {
            current.steps.push(S(AstBlockStep::AstBlockStepError, *span));
          }
          Statement::Return => {
            // new blocks default to Return as their control flow, but the
            // creator of the current block *might* have set something else, so
            // we have to be sure to set it back to Return.
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
                current.next = AstBlockFlow::Always(*here);
              }
            }
            if statement_iter.peek().is_some() {
              // TODO: unreachable code warning.
            }
            break 'statement_walk;
          }
          Statement::IfElse(if_else) => {
            let mut if_block = AstBlock::new();
            let mut else_block = AstBlock::new();
            let mut after_block = AstBlock::new();
            after_block.next = core::mem::replace(
              &mut current.next,
              AstBlockFlow::Branch(
                if_else.condition.clone(),
                if_block.id,
                else_block.id,
              ),
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
            after_block.next = core::mem::replace(
              &mut current.next,
              AstBlockFlow::Always(here_block.id),
            );
            here_block.next = AstBlockFlow::Always(here_block.id);
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
  let mut out = vec![AstBlock::new()];
  let mut label_stack = Vec::new();
  recursive_inner(statements, &mut out, &mut label_stack);
  out
}

#[derive(Debug, Clone)]
pub struct SsaBlock {
  pub id: BlockID,
  pub steps: Vec<S<SsaBlockStep>>,
  pub next: SsaBlockFlow,
}

#[derive(Debug, Clone, Default)]
pub enum SsaBlockFlow {
  /// Jump to this block.
  Always(BlockID),
  /// Branch on the carry flag
  BranchCarry(BlockID, BlockID),
  /// Branch on the zero flag
  BranchZero(BlockID, BlockID),
  /// return the caller.
  Return,
  /// Incomplete expr to ssa transformation
  #[default]
  SsaBlockFlowError,
}

/// a variable and its version
#[derive(Clone, Copy)]
pub struct SsaVar(pub SsaVarName,pub usize);
impl core::fmt::Debug for SsaVar {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    core::fmt::Debug::fmt(&self.0, f)?;
    f.write_str("{")?;
    core::fmt::Debug::fmt(&self.1, f)?;
    f.write_str("}")?;
    Ok(())
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(u8)]
pub enum SsaVarName {
  A, B, C, D, E, H, L, Hlm, Mem, Temp, ZeroF, CarryF
}

#[derive(Debug, Clone, Default)]
pub struct SsaVarMaker(HashMap<SsaVarName, usize>);
impl SsaVarMaker {
  #[inline]
  pub fn next_var(&mut self, name: SsaVarName) -> SsaVar {
    let x = self.0.entry(name).or_default();
    *x += 1;
    SsaVar(name, *x)
  }
  #[inline]
  pub fn latest_var(&mut self, name: SsaVarName) -> SsaVar {
    let x = self.0.entry(name).or_default();
    SsaVar(name, *x)
  }
}

#[derive(Debug, Clone, Default)]
pub enum SsaBlockStep {
  #[default]
  SsaBlockStepError,
  
  /// Sets a variable to a constant value.
  SetImm(SsaVar, i32),
  
  /// `zero = op_output_was_zero`
  FlagZeroFromOp(SsaVar, SsaVar),
  
  /// `carry = op_output_carried`
  FlagCarryFromOp(SsaVar, SsaVar),
  
  /// Store `a` to a const address.
  Store(u16, SsaVar),
  
  /// Load from a const address into `a`.
  Load(SsaVar, u16),
  
  /// `flags = a - const`
  CmpImm(SsaVar, SsaVar, i32),
  
  /// `b = b++`
  ///
  /// * 8-bit: assign zero based on output
  /// * 16-bit: no flag effects
  Inc(SsaVar, SsaVar),
  
  /// `b = b--`
  ///
  /// * 8-bit: assign zero based on output
  /// * 16-bit: no flag effects
  Dec(SsaVar, SsaVar),
}

pub fn split_ast_to_ssa(ast_block: &AstBlock, maker: &mut SsaVarMaker) -> SsaBlock {
  let mut ssa_block = SsaBlock {
    id: ast_block.id,
    steps: Vec::new(),
    next: SsaBlockFlow::SsaBlockFlowError,
  };
  
  // todo: ssa steps from ast steps
  for S(step, span) in ast_block.steps.iter() {
    let span = *span;
    match step {
      AstBlockStep::AstBlockStepError => {
        ssa_block.steps.push(S(SsaBlockStep::SsaBlockStepError, span));
      }
      AstBlockStep::Call(_id) => {
        dbg!("TODO: calls");
        ssa_block.steps.push(S(SsaBlockStep::SsaBlockStepError, span));
      }
      AstBlockStep::Expr(expr) => {
        match expr {
          Expr::Assign(ref_box) => {
            let a2: &[S<Expr>;2] = &*ref_box;
            let [S(dst, _dst_span), S(src, _src_span)] = a2;
            match dst {
              Expr::Reg(dst_reg) => {
                match src {
                  Expr::NumLit(str_id) => {
                    match parse_num_lit(*str_id){
                      Some(i)=>{
                        match dst_reg {
                          Register::A => {
                            ssa_block.steps.push(
                            S(SsaBlockStep::SetImm(
                           maker.next_var(SsaVarName::A), i ),span));
                          }
                          Register::B => {
                            ssa_block.steps.push(
                            S(SsaBlockStep::SetImm(
                           maker.next_var(SsaVarName::B), i ),span));
                          }
                          Register::C => {
                            ssa_block.steps.push(
                            S(SsaBlockStep::SetImm(
                           maker.next_var(SsaVarName::C), i ),span));
                          }
                          other_dst => {
                            dbg!(&other_dst);
                            ssa_block.steps.push(S(SsaBlockStep::SsaBlockStepError, span));
                          }
                        }
                      }
                      None => {
                        // todo: log error 
                        ssa_block.steps.push(S(SsaBlockStep::SsaBlockStepError, span));
                      }
                    }
                  }
                  other_src => {
                    dbg!(&dst_reg, &other_src);
                    ssa_block.steps.push(S(SsaBlockStep::SsaBlockStepError, span));
                  }
                }
              }
              Expr::Deref(deref_expr) => match deref_expr.as_slice() {
                [S(Expr::NumLit(str_id), _dst_span)] => {
                  match parse_num_lit(*str_id).and_then(|i|u16::try_from(i).ok()){
                    Some(u)=>{
                      match src {
                        Expr::Reg(Register::A) =>{
                          ssa_block.steps.push(
                        S(SsaBlockStep::Store(u,maker.latest_var(SsaVarName::A)), *_dst_span
                        ))
                        }
                        other_src => {
                          dbg!(&other_src);
                        }
                      }
                      
                    }
                    None => {
                      // todo: log error 
                      ssa_block.steps.push(S(SsaBlockStep::SsaBlockStepError, span));
                    }
                  }
                }
                other_deref => match src {
                  other_src => {
                    dbg!(&other_deref, &other_src);
                    ssa_block.steps.push(S(SsaBlockStep::SsaBlockStepError, span));
                  }
                }
              }
              other_dst => match src {
                other_src => {
                  dbg!(&other_dst, &other_src);
                  ssa_block.steps.push(S(SsaBlockStep::SsaBlockStepError, span));
                }
              }
            }
          }
          other_expr => {
            dbg!(&other_expr);
            ssa_block.steps.push(S(SsaBlockStep::SsaBlockStepError, span));
          }
        }
      }
    }
  }
  // set the correct flow data
  match &ast_block.next {
    AstBlockFlow::Return => {
      ssa_block.next = SsaBlockFlow::Return;
    }
    AstBlockFlow::Always(id) => {
      ssa_block.next = SsaBlockFlow::Always(*id);
    }
    AstBlockFlow::Branch(S(_expr, _span), _t, _f) => {
      // todo: turn expr into ssa and a branch on the correct flag.
    }
  }
  
  //
  ssa_block
}

fn parse_num_lit(str_id: StrID) -> Option<i32> {
  let s = str_id.as_str();
  let mut t = 0_i32;
  if let Some(hex_str) = s.strip_prefix('$') {
    // hexadecimal
    for c in hex_str.chars() {
      match c.to_ascii_lowercase() {
        '_' => continue,
        'a'..='f' => {
          t *= 16;
          t += (c as u8 - 'a' as u8) as i32;
        }
        '0'..='9' => {
          t *= 16;
          t += (c as u8 - '0' as u8) as i32;
        }
        _ => return None,
      }
    }
  } else if let Some(bin_str) = s.strip_prefix('%') {
    // binary
    for c in bin_str.chars() {
      match c {
        '_' => continue,
        '0'..='1' => {
          t *= 2;
          t += (c as u8 - '0' as u8) as i32;
        }
        _ => return None,
      }
    }
  } else {
    // decimal
    for c in s.chars() {
      match c {
        '_' => continue,
        '0'..='9' => {
          t *= 10;
          t += (c as u8 - '0' as u8) as i32;
        }
        _ => return None,
      }
    }
  }
  Some(t)
}
