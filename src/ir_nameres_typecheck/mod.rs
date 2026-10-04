//! "Grant me the power of name resolution!"

use fnv::FnvHashMap;

use crate::ast::visitor::TreeVisitMut;
use crate::ast::{Ast, PointerAccessKind};
use crate::ir_nameres_typecheck::min_const_eval::ConstEvaluator;
use crate::ir_nameres_typecheck::name_res::NameResolver;
use crate::{FnvBiHashMap, ItemId, TypeId, ValueExprId};

pub mod min_const_eval;
pub mod name_res;
pub mod type_check;

#[derive(Debug, Clone)]
pub struct IrNameResTypeCheck {
  pub ast: Ast,
  pub type_database: FnvBiHashMap<TypeId, Type>,
  pub vx_id_to_ty_id: FnvHashMap<ValueExprId, TypeId>,
}
impl IrNameResTypeCheck {
  pub fn new(ast: Ast) -> Self {
    let mut out = Self {
      ast,
      type_database: Default::default(),
      vx_id_to_ty_id: Default::default(),
    };
    out
  }

  pub fn resolve_names(&mut self) {
    let mut name_resolver = NameResolver::default();
    name_resolver.walk_ast(&mut self.ast);
  }

  pub fn run_const_eval(&mut self) {
    // we have to temporarily juggle the Ast out of the Ir itself so that we can
    // walk it separately from the other IR data.
    let mut ast = core::mem::take(&mut self.ast);
    let lit_id = self.type_id_of(&Type::LiteralInteger);
    let mut evaluator = ConstEvaluator {
      errors: Vec::new(),
      file_origin: None,
      ir: self,
      lit_id,
    };
    evaluator.walk_ast(&mut ast);
    let ConstEvaluator { ir, file_origin: _, lit_id: _, errors } = evaluator;
    ast.errors.extend(errors);
    core::mem::swap(&mut ast, &mut ir.ast);
  }

  pub fn type_id_of(&mut self, ty: &Type) -> TypeId {
    match self.type_database.get_by_right(ty) {
      Some(id) => *id,
      None => {
        let id = TypeId::new();
        self.type_database.insert(id, ty.clone());
        id
      }
    }
  }
}

/// The types that a local variable can be.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
  ErrType,
  Never,
  LiteralInteger,
  Unit,
  Bool,
  U8,
  I8,
  U16,
  I16,
  Array { elem: TypeId, elem_count: u32 },
  Pointer { access_kind: PointerAccessKind, target: TypeId },
  Function { args: Vec<TypeId>, ret: TypeId },
  Struct(ItemId),
  Bitbag(ItemId),
  Enum(ItemId),
}
