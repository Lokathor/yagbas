use chumsky::span::SimpleSpan;

#[derive(Debug, Clone)]
pub struct Ast {
  pub items: Vec<(Item, SimpleSpan)>,
}

#[derive(Debug, Clone)]
pub enum Item {
  BitStruct(BitStruct),
  Const(Const),
  Func(Func),
  Static(Static),
  Struct(Struct),
  ItemError,
}

#[derive(Debug, Clone)]
pub struct BitStruct {
  file_id: FileID,
  name: StrID,
  /// `field_name: bit` list
  fields: Vec<(StrID, StrID)>,
}

#[derive(Debug, Clone)]
pub struct Const {
  file_id: FileID,
  name: StrID,
  expr: Expr,
}

#[derive(Debug, Clone)]
pub struct Func {
  file_id: FileID,
  name: StrID,
  body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct Static {
  file_id: FileID,
  name: StrID,
  expr: Expr,
}

#[derive(Debug, Clone)]
pub struct Struct {
  file_id: FileID,
  name: StrID,
  /// `field_name: field_type` list
  fields: Vec<(StrID, StrID)>,
}

#[derive(Debug, Clone)]
pub enum Expr {
  // TODO
  ExprError,
}

#[derive(Debug, Clone)]
pub enum Statement {
  Expr(Expr),
  Loop(Loop),
  IfElse(IfElse),
  StatementError,
}

#[derive(Debug, Clone)]
pub struct Loop {
  name: Option<StrID>,
  body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct IfElse {
  condition: Expr,
  if_body: Vec<Statement>,
  else_body: Vec<Statement>,
}
