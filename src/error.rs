use super::*;
use ariadne::{Cache, CharSet, Config, FnCache, Label, Report, ReportKind};
use chumsky::error::{RichPattern, RichReason};
use core::iter::IntoIterator;
use std::sync::{Mutex, PoisonError};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum YagError {
  IO(PathBuf, String),
  TokenTreeParseError(FileID, Rich<'static, Token, Span32>),
  ItemParseError(FileID, Rich<'static, TokenTree, Span32>),
}

pub static ERROR_BUCKET: Mutex<Vec<YagError>> = Mutex::new(Vec::new());

/// Calls `log_error_iter` for a single error.
pub fn log_error(e: YagError) {
  log_error_iter([e]);
}

pub fn log_error_iter<I: IntoIterator<Item = YagError>>(i: I) {
  let mut locked_vec =
    ERROR_BUCKET.lock().unwrap_or_else(PoisonError::into_inner);
  locked_vec.extend(i);
}

/// Returns `true` if there is an error printed.
pub fn print_any_errors() -> bool {
  let mut cache = file_data_cache_sources();
  let config = Config::new().with_char_set(CharSet::Ascii).with_color(false);

  let mut locked_vec =
    ERROR_BUCKET.lock().unwrap_or_else(PoisonError::into_inner);
  locked_vec.sort();
  for e in locked_vec.iter() {
    match e {
      YagError::IO(_, _) => eprintln!("{e:?}"),
      YagError::TokenTreeParseError(_, _) => eprintln!("{e:?}"),
      YagError::ItemParseError(file_id, rich) => {
        let a_span =
          (*file_id, (rich.span().start as usize)..(rich.span().end as usize));
        let mut report = Report::build(ReportKind::Error, a_span.clone());
        report = report.with_config(config);
        let found = if let Some(tt) = rich.found() {
          match tt {
            TokenTree::Lone(token) => format!(
              "{}",
              SourcedTokens(file_id.get_data().content(), *rich.span(), *token)
            ),
            TokenTree::Parens(_) => String::from("Parenthesis Group"),
            TokenTree::Brackets(_) => String::from("Bracket Group"),
            TokenTree::Braces(_) => String::from("Braces Group"),
            TokenTree::TreeError => String::from("TokenTreeError"),
          }
        } else {
          String::from("End Of Input")
        };
        report = report.with_message(format!(
          "found {found}, but expected one of {ex:?}",
          ex = rich.expected().collect::<Vec<_>>(),
        ));
        report =
          report.with_label(Label::new(a_span).with_message(format!("here")));
        for (pat, span) in rich.contexts() {
          let b_span = (*file_id, (span.start as usize)..(span.end as usize));
          report = report
            .with_label(Label::new(b_span).with_message(format!("{pat:?}")))
        }
        //
        report.finish().eprint(&mut cache).unwrap();
      }
    }
  }
  !locked_vec.is_empty()
}
