use std::collections::HashMap;

use slotmap::{SlotMap, new_key_type};
use str_id::StrId;

use crate::{
  Span,
  ast::{
    Ast, AstBody, AstExprType, AstExprTypeKind, AstExprVal, AstExprValKind,
    AstItem, AstItemKind, AstLet, AstModule, AstStatementKind,
  },
  operators::BinOpKind,
};

new_key_type! {
  pub struct NameId;
}

new_key_type! {
  pub struct TypeId;
}

#[derive(Debug, Clone, Copy)]
pub struct NameInfo {
  pub file_origin: StrId,
  pub span: Span,
  pub text: StrId,
  pub kind: NameKind,
}

#[derive(Debug, Clone, Copy)]
pub enum NameKind {
  StaticMmio,
  Constant,
  Function,
  FunctionArgument,
  LetVariable,
}

#[derive(Debug, Clone, Copy)]
pub struct TypeInfo {
  pub file_origin: StrId,
  pub span: Span,
  pub text: StrId,
  pub kind: TypeKind,
}

#[derive(Debug, Clone, Copy)]
pub enum TypeKind {
  Plain,
  Array,
  Pointer,
}

/// Intermediate Representation with Name Resolution
#[derive(Debug, Clone, Default)]
pub struct IrNameres {
  pub ast: Ast,
  pub names: SlotMap<NameId, NameInfo>,
  pub types: SlotMap<TypeId, TypeInfo>,
}
impl IrNameres {
  pub fn from_ast(ast: Ast) -> Self {
    let mut out = Self::default();
    out.ast = ast;
    //
    let mut name_resolver = VarNameResolver {
      names: &mut out.names,
      file_origin: StrId::default(),
      scopes: &mut Vec::new(),
    };
    name_resolver.resolve_for_ast(&mut out.ast);
    //
    let mut type_resolver = TypeNameResolver {
      types: &mut out.types,
      file_origin: StrId::default(),
      scopes: &mut Vec::new(),
    };
    type_resolver.init();
    type_resolver.resolve_for_ast(&mut out.ast);
    //
    out
  }
}

#[derive(Debug)]
struct VarNameResolver<'a> {
  names: &'a mut SlotMap<NameId, NameInfo>,
  file_origin: StrId,
  scopes: &'a mut Vec<HashMap<StrId, NameId>>,
}
impl<'a> VarNameResolver<'a> {
  fn resolve_for_ast(&mut self, ast: &mut Ast) {
    for module in ast.modules.iter_mut() {
      self.scopes.push(HashMap::new());
      self.file_origin = module.file_origin;
      self.resolve_for_module(module);
      self.scopes.pop();
    }
  }

  fn resolve_for_module(&mut self, module: &mut AstModule) {
    for item in module.items.iter() {
      self.resolve_item_exterior(item);
    }

    for item in module.items.iter_mut() {
      self.scopes.push(HashMap::new());
      self.resolve_item_interior(item);
      self.scopes.pop();
    }
  }

  fn resolve_item_exterior(&mut self, item: &AstItem) {
    let (text, span, kind) = match &item.kind {
      AstItemKind::ErrAstItemKind => return,
      AstItemKind::StaticMmio(ast_static_mmio) => {
        (ast_static_mmio.name, ast_static_mmio.name_span, NameKind::StaticMmio)
      }
      AstItemKind::Constant(ast_constant) => {
        (ast_constant.name, ast_constant.name_span, NameKind::Constant)
      }
      AstItemKind::Function(ast_function) => {
        (ast_function.name, ast_function.name_span, NameKind::Function)
      }
    };
    let info = NameInfo { file_origin: self.file_origin, span, text, kind };
    let name_key = self.names.insert(info);
    if let Some(_old) = self.scopes.last_mut().unwrap().insert(text, name_key) {
      // TODO: error here, more than one symbol share the same name at this
      // scope level.
    }
  }

  fn resolve_item_interior(&mut self, item: &mut AstItem) {
    match &mut item.kind {
      AstItemKind::ErrAstItemKind => return,
      AstItemKind::StaticMmio(ast_static_mmio) => {
        self.resolve_for_expr(&mut ast_static_mmio.address);
      }
      AstItemKind::Constant(ast_constant) => {
        self.resolve_for_expr(&mut ast_constant.xpr);
      }
      AstItemKind::Function(ast_function) => {
        for arg in ast_function.arguments.iter() {
          let info = NameInfo {
            file_origin: self.file_origin,
            span: arg.name_span,
            text: arg.name,
            kind: NameKind::FunctionArgument,
          };
          let name_key = self.names.insert(info);
          if let Some(_old) =
            self.scopes.last_mut().unwrap().insert(arg.name, name_key)
          {
            // TODO: error here, more than one symbol share the same name at
            // scope level.
          }
        }
        self.scopes.push(HashMap::new());
        self.resolve_for_body(&mut ast_function.body);
        self.scopes.push(HashMap::new());
      }
    }
  }

  fn resolve_for_body(&mut self, body: &mut AstBody) {
    // first any items defined here are resolved as if they were defined at the
    // start of the body.
    for stmt in body.statements.iter() {
      match &stmt.kind {
        AstStatementKind::Item(ast_item) => {
          self.resolve_item_exterior(ast_item);
        }
        _ => continue,
      }
    }
    // next we step inside each thing we find, including stepping inside any
    // items we find.
    for stmt in body.statements.iter_mut() {
      match &mut stmt.kind {
        AstStatementKind::ErrAstStatementKind => continue,
        AstStatementKind::Let(ast_let) => self.resolve_for_let(ast_let),
        AstStatementKind::Expression(ast_expr_val) => {
          self.resolve_for_expr(ast_expr_val);
        }
        AstStatementKind::Item(ast_item) => {
          self.scopes.push(HashMap::new());
          self.resolve_item_interior(ast_item);
          self.scopes.pop();
        }
      }
    }
  }

  /// resolve for let statement.
  ///
  /// * resolve the expression of the let
  /// * make the new name in the let pattern.
  /// * replace that new name with the reaolved form.
  fn resolve_for_let(&mut self, ast_let: &mut AstLet) {
    self.resolve_for_expr(&mut ast_let.xpr);
    match &ast_let.pattern.kind {
      AstExprValKind::Identifier(i) => {
        let info = NameInfo {
          file_origin: self.file_origin,
          text: *i,
          span: ast_let.pattern.span,
          kind: NameKind::LetVariable,
        };
        let name_key = self.names.insert(info);
        if let Some(_old) = self.scopes.last_mut().unwrap().insert(*i, name_key)
        {
          // here the new let definition shadows a previous one at the same scope,
          // which is allowed. some day maybe a pedantic warning?
        }
      }
      _other => {
        dbg!(_other);
        return;
      }
    }
    self.resolve_for_expr(&mut ast_let.pattern);
  }

  fn resolve_for_expr(&mut self, xpr: &mut AstExprVal) {
    match &mut xpr.kind {
      AstExprValKind::Identifier(i) => {
        if let Some((_, n)) = self
          .scopes
          .iter()
          .rev()
          .flat_map(|hm| hm.iter())
          .find(|(s, _n)| i == *s)
        {
          xpr.kind = AstExprValKind::ResolvedName(*n);
        } else {
          // TODO: log error
          eprintln!("Not In Scope: {} at ({:?})", *i, xpr.span);
        }
      }
      AstExprValKind::If(data) => {
        self.resolve_for_expr(&mut data.condition);
        //
        self.scopes.push(HashMap::new());
        self.resolve_for_body(&mut data.if_body);
        self.scopes.push(HashMap::new());
        //
        self.scopes.push(HashMap::new());
        self.resolve_for_body(&mut data.else_body);
        self.scopes.push(HashMap::new());
      }
      AstExprValKind::Loop(ast_body) => {
        self.scopes.push(HashMap::new());
        self.resolve_for_body(ast_body);
        self.scopes.push(HashMap::new());
      }
      AstExprValKind::For(data) => {
        self.resolve_for_expr(&mut data.range_expr);
        self.scopes.push(HashMap::new());
        {
          match &data.step_expr.kind {
            AstExprValKind::Identifier(i) => {
              let info = NameInfo {
                file_origin: self.file_origin,
                text: *i,
                span: data.step_expr.span,
                kind: NameKind::LetVariable,
              };
              let name_key = self.names.insert(info);
              if let Some(_old) =
                self.scopes.last_mut().unwrap().insert(*i, name_key)
              {
                // here the new let definition shadows a previous one at the same scope,
                // which is allowed. some day maybe a pedantic warning?
              }
            }
            _other => {
              dbg!(_other);
              return;
            }
          }
          self.resolve_for_expr(&mut data.step_expr);
          self.resolve_for_body(&mut data.body);
        }
        self.scopes.push(HashMap::new());
      }
      AstExprValKind::UnOp(_, inner) => {
        self.resolve_for_expr(inner);
      }
      AstExprValKind::BinOp(data) => {
        if data.op == BinOpKind::Access {
          self.resolve_for_expr(&mut data.left);
          // skip right side for now, we don't know the type yet.
        } else {
          self.resolve_for_expr(&mut data.left);
          self.resolve_for_expr(&mut data.right);
        }
      }
      AstExprValKind::LiteralNumber(_)
      | AstExprValKind::ResolvedName(_)
      | AstExprValKind::Break => return,
      _other => {
        dbg!(&_other);
      }
    }
  }
}

#[derive(Debug)]
struct TypeNameResolver<'a> {
  types: &'a mut SlotMap<TypeId, TypeInfo>,
  file_origin: StrId,
  scopes: &'a mut Vec<HashMap<StrId, TypeId>>,
}
impl<'a> TypeNameResolver<'a> {
  fn init(&mut self) {
    self.scopes.push(HashMap::new());
    let mut info = TypeInfo {
      file_origin: StrId::from("builtin"),
      span: Span::default(),
      text: StrId::default(),
      kind: TypeKind::Plain,
    };
    let x = ["()", "bool", "u8", "i8", "u16", "i16", "fx8_8"];
    for s in x {
      info.text = StrId::from(s);
      let type_key = self.types.insert(info);
      if let Some(_old) =
        self.scopes.last_mut().unwrap().insert(info.text, type_key)
      {
        panic!("builtin type defined more than once: `{s}`");
      }
    }
  }

  fn resolve_for_ast(&mut self, ast: &mut Ast) {
    for module in ast.modules.iter_mut() {
      self.scopes.push(HashMap::new());
      self.file_origin = module.file_origin;
      self.resolve_for_module(module);
      self.scopes.pop();
    }
  }

  fn resolve_for_module(&mut self, module: &mut AstModule) {
    for item in module.items.iter() {
      self.resolve_item_exterior(item);
    }

    for item in module.items.iter_mut() {
      self.scopes.push(HashMap::new());
      self.resolve_item_interior(item);
      self.scopes.pop();
    }
  }

  fn resolve_item_exterior(&mut self, _item: &AstItem) {
    return;
    // todo: add new types when a struct or bitbag is defined.
  }

  fn resolve_item_interior(&mut self, item: &mut AstItem) {
    match &mut item.kind {
      AstItemKind::ErrAstItemKind => return,
      AstItemKind::StaticMmio(ast_static_mmio) => {
        self.resolve_for_ty(&mut ast_static_mmio.ty);
        self.resolve_for_expr(&mut ast_static_mmio.address);
      }
      AstItemKind::Constant(ast_constant) => {
        self.resolve_for_ty(&mut ast_constant.ty);
        self.resolve_for_expr(&mut ast_constant.xpr);
      }
      AstItemKind::Function(ast_function) => {
        for arg in ast_function.arguments.iter_mut() {
          self.resolve_for_ty(&mut arg.ty);
        }
        self.scopes.push(HashMap::new());
        self.resolve_for_body(&mut ast_function.body);
        self.scopes.push(HashMap::new());
      }
    }
  }

  fn resolve_for_body(&mut self, body: &mut AstBody) {
    // first any items defined here are resolved as if they were defined at the
    // start of the body.
    for stmt in body.statements.iter() {
      match &stmt.kind {
        AstStatementKind::Item(ast_item) => {
          self.resolve_item_exterior(ast_item);
        }
        _ => continue,
      }
    }
    // next we step inside each thing we find, including stepping inside any
    // items we find.
    for stmt in body.statements.iter_mut() {
      match &mut stmt.kind {
        AstStatementKind::ErrAstStatementKind => continue,
        AstStatementKind::Let(ast_let) => self.resolve_for_let(ast_let),
        AstStatementKind::Expression(ast_expr_val) => {
          self.resolve_for_expr(ast_expr_val);
        }
        AstStatementKind::Item(ast_item) => {
          self.scopes.push(HashMap::new());
          self.resolve_item_interior(ast_item);
          self.scopes.pop();
        }
      }
    }
  }

  /// resolve for let statement.
  ///
  /// * resolve the expression of the let
  /// * make the new name in the let pattern.
  /// * replace that new name with the reaolved form.
  fn resolve_for_let(&mut self, ast_let: &mut AstLet) {
    self.resolve_for_expr(&mut ast_let.xpr);
    self.resolve_for_expr(&mut ast_let.pattern);
  }

  fn resolve_for_expr(&mut self, xpr: &mut AstExprVal) {
    match &mut xpr.kind {
      AstExprValKind::If(data) => {
        self.resolve_for_expr(&mut data.condition);
        //
        self.scopes.push(HashMap::new());
        self.resolve_for_body(&mut data.if_body);
        self.scopes.push(HashMap::new());
        //
        self.scopes.push(HashMap::new());
        self.resolve_for_body(&mut data.else_body);
        self.scopes.push(HashMap::new());
      }
      AstExprValKind::Loop(ast_body) => {
        self.scopes.push(HashMap::new());
        self.resolve_for_body(ast_body);
        self.scopes.push(HashMap::new());
      }
      AstExprValKind::For(data) => {
        self.resolve_for_expr(&mut data.range_expr);
        self.scopes.push(HashMap::new());
        {
          self.resolve_for_expr(&mut data.step_expr);
          self.resolve_for_body(&mut data.body);
        }
        self.scopes.push(HashMap::new());
      }
      AstExprValKind::UnOp(_, inner) => {
        self.resolve_for_expr(inner);
      }
      AstExprValKind::BinOp(data) => {
        if data.op == BinOpKind::Access {
          self.resolve_for_expr(&mut data.left);
          // skip right side for now, we don't know the type yet.
        } else {
          self.resolve_for_expr(&mut data.left);
          self.resolve_for_expr(&mut data.right);
        }
      }
      AstExprValKind::LiteralNumber(_)
      | AstExprValKind::ResolvedName(_)
      | AstExprValKind::Break => return,
      _other => {
        dbg!(&_other);
      }
    }
  }

  fn resolve_for_ty(&mut self, ty: &mut AstExprType) {
    match &ty.kind {
      AstExprTypeKind::Plain(t) => {
        if let Some((_, n)) = self
          .scopes
          .iter()
          .rev()
          .flat_map(|hm| hm.iter())
          .find(|(s, _n)| t == *s)
        {
          ty.kind = AstExprTypeKind::ResolvedType(*n);
        } else {
          // TODO: log error
          eprintln!("Not In Scope: {} at ({:?})", *t, ty.span);
        }
      }
      AstExprTypeKind::Array { .. } => {
        // TODO: we need to evaluate constants if they're in the array definition. So for now we have to wait on defining all the array types.
      }
      _other => {
        dbg!(&_other);
      }
    }
  }
}
