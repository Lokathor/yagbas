use ariadne::{Cache, CharSet, Config, Label, Report, ReportKind};
use chumsky::error::Rich;

use crate::{
  ast::{token::Token, token_tree::TokenTree, Item, ItemKind},
  src_files::{FileSpan, FileSpanned, SrcID},
  str_id::StrID,
};
use core::fmt::Write;
use std::collections::{hash_map::Entry, HashMap};

mod multiple_definition;
pub use multiple_definition::*;

mod call_target_illegal;
pub use call_target_illegal::*;

mod break_continue_illegal;
pub use break_continue_illegal::*;

#[derive(Debug, Clone)]
pub enum YagError {
  FileIO { filename: String, message: String },
  Tokenization(FileSpan),
  TokenTree(Rich<'static, Token, FileSpan, &'static str>),
  Item(Rich<'static, TokenTree, FileSpan, &'static str>),
  MultipleDefinition(MultipleDefinition),
  CallTargetIllegal(CallTargetIllegal),
  BreakContinueIllegal(BreakContinueIllegal),
}
impl YagError {
  pub fn one_line(&self) -> String {
    match self {
      YagError::FileIO { filename, message } => {
        format!("Error: {filename}: IO: {message}")
      }
      YagError::Tokenization(file_span) => {
        format!("Error: Tokenization: {file_span:?}")
      }
      YagError::TokenTree(rich) => format!("Error: Token Tree: {rich:?}"),
      YagError::Item(rich) => format!("Error: Item: {rich:?}"),
      YagError::MultipleDefinition(multiple_definition) => {
        multiple_definition.one_line()
      }
      YagError::CallTargetIllegal(call_target_legal) => {
        call_target_legal.one_line()
      }
      YagError::BreakContinueIllegal(break_continue_illegal) => {
        break_continue_illegal.one_line()
      }
    }
  }

  pub fn build_report(&self, config: Config) -> Report<FileSpan> {
    match self {
      YagError::FileIO { filename, message } => {
        Report::build(ReportKind::Error, FileSpan::default())
          .with_config(config)
          .with_message(format!("{filename}: IO: {message}"))
          .finish()
      }
      YagError::Tokenization(file_span) => {
        Report::build(ReportKind::Error, *file_span)
          .with_config(config)
          .with_message("Tokenization Error")
          .with_label(Label::new(*file_span))
          .finish()
      }
      YagError::TokenTree(rich) => {
        Report::build(ReportKind::Error, *rich.span())
          .with_config(config)
          .with_message(format!("{rich:?}"))
          .with_labels(rich.contexts().map(|(context, file_span)| {
            Label::new(*file_span)
              .with_message(format!("while parsing {context}"))
          }))
          .finish()
      }
      YagError::Item(rich) => Report::build(ReportKind::Error, *rich.span())
        .with_config(config)
        .with_message(format!("{rich:?}"))
        .with_labels(rich.contexts().map(|(context, file_span)| {
          Label::new(*file_span)
            .with_message(format!("while parsing {context}"))
        }))
        .finish(),
      YagError::MultipleDefinition(multiple_definition) => {
        multiple_definition.build_report(config)
      }
      YagError::CallTargetIllegal(call_target_legal) => {
        call_target_legal.build_report(config)
      }
      YagError::BreakContinueIllegal(break_continue_illegal) => {
        break_continue_illegal.build_report(config)
      }
    }
  }
}
