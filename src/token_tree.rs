use chumsky::{
  input::{Input, SpannedInput, ValueInput},
  prelude::*,
};

use crate::{
  src_files::FileSpan,
  token::Token::{self, *},
  ErrRichToken, TokenSliceInput,
};

/// A lone token or a list of token trees within one of three groupings.
///
/// Collecting a raw token list into token trees ensures that all the
/// opening/closing markers of all the groupings are balanced before trying to
/// do any more advanced parsing.
#[derive(Clone, PartialEq, Eq)]
pub enum TokenTree {
  Lone(Token),
  Parens(Vec<(TokenTree, FileSpan)>),
  Brackets(Vec<(TokenTree, FileSpan)>),
  Braces(Vec<(TokenTree, FileSpan)>),
  TreeError,
}
impl TokenTree {
  /// Parses for just one token tree.
  pub fn parser<'a>(
  ) -> impl Parser<'a, TokenSliceInput<'a>, Self, ErrRichToken<'a>> + Clone {
    recursive(|tt| {
      let token_list = tt
        .map_with(|token, ex| (token, ex.span()))
        .repeated()
        .collect::<Vec<(TokenTree, FileSpan)>>();

      // Looks like `[ ... ]`
      let open_bracket = select! {
        OpBracket => (),
      };
      let close_bracket = select! {
        ClBracket => (),
      };
      let brackets = token_list
        .clone()
        .delimited_by(open_bracket, close_bracket)
        .map(TokenTree::Brackets);

      // Looks like `{ ... }`
      let open_brace = select! {
        OpBrace => (),
      };
      let close_brace = select! {
        ClBrace => (),
      };
      let braces = token_list
        .clone()
        .delimited_by(open_brace, close_brace)
        .map(TokenTree::Braces);

      // Looks like `( ... )`
      let open_paren = select! {
        OpParen => (),
      };
      let close_paren = select! {
        ClParen => (),
      };
      let parens = token_list
        .clone()
        .delimited_by(open_paren, close_paren)
        .map(TokenTree::Parens);

      // Looks like something that does *NOT* open or close one of the other
      // types.
      let single =
        none_of([OpBracket, ClBracket, OpBrace, ClBrace, OpParen, ClParen])
          .map(TokenTree::Lone);

      // Looks like `//`
      let single_comment = select! {
        CommentSingle => (),
      };

      // Looks like `/* ... */`
      let block_start = select! {
        CommentBlockStart => (),
      };
      let block_end = select! {
        CommentBlockEnd => (),
      };
      let block_comment =
        token_list.clone().delimited_by(block_start, block_end).ignored();

      // Either type of comment
      let comment = single_comment.or(block_comment);

      let x = choice((brackets, braces, parens, single))
        .padded_by(comment.repeated());

      x
    })
  }
}

#[allow(clippy::type_complexity)]
pub fn parse_tokens_to_token_trees(
  tokens: &[(Token, FileSpan)],
) -> (Vec<(TokenTree, FileSpan)>, Vec<Rich<'static, Token, FileSpan>>) {
  if tokens.is_empty() {
    return (Vec::new(), Vec::new());
  }
  let last_span = *tokens.last().map(|(_, s)| s).unwrap();
  let end_span = FileSpan { start: last_span.end, ..last_span };
  let recover_strategy =
    via_parser(any().repeated().at_least(1).to(TokenTree::TreeError));
  let parser = TokenTree::parser()
    .recover_with(recover_strategy)
    .map_with(|token_tree, ex| (token_tree, ex.span()))
    .repeated()
    .collect::<Vec<(TokenTree, FileSpan)>>();
  let input = tokens.spanned(end_span);
  let (trees, errors) = parser.parse(input).into_output_errors();
  (
    trees.unwrap_or_default(),
    errors.into_iter().map(|r| r.into_owned()).collect::<Vec<_>>(),
  )
}

impl core::fmt::Debug for TokenTree {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let skip_threshold = 100;
    match self {
      Self::Lone(t) => core::fmt::Debug::fmt(&t, f),
      Self::Parens(ts) => {
        if ts.len() > skip_threshold {
          write!(f, "(...{} elements...)", ts.len())
        } else {
          write!(f, "(")?;
          for (i, (tt, _span)) in ts.iter().enumerate() {
            if i > 0 {
              write!(f, " ")?;
            }
            write!(f, "{tt:?}")?;
          }
          write!(f, ")")?;
          Ok(())
        }
      }
      Self::Brackets(ts) => {
        if ts.len() > skip_threshold {
          write!(f, "[...{} elements...]", ts.len())
        } else {
          write!(f, "[")?;
          for (i, (tt, _span)) in ts.iter().enumerate() {
            if i > 0 {
              write!(f, " ")?;
            }
            write!(f, "{tt:?}")?;
          }
          write!(f, "]")?;
          Ok(())
        }
      }
      Self::Braces(ts) => {
        if ts.len() > skip_threshold {
          write!(f, "{{...{} elements...}}", ts.len())
        } else {
          write!(f, "{{")?;
          for (i, (tt, _span)) in ts.iter().enumerate() {
            if i > 0 {
              write!(f, " ")?;
            }
            write!(f, "{tt:?}")?;
          }
          write!(f, "}}")?;
          Ok(())
        }
      }
      Self::TreeError => write!(f, "TreeError"),
    }
  }
}
