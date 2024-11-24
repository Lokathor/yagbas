use super::*;

use token::Token;

#[derive(Debug, Clone)]
pub struct IfElse {
  pub test: Vec<FileSpanned<Token>>,
  pub if_body: Vec<FileSpanned<Statement>>,
  pub else_body: Vec<FileSpanned<Statement>>,
}
