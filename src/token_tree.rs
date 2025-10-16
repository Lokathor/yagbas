use super::*;
use chumsky::{
  extra::{Err, ParserExtra},
  input::{BorrowInput, ValueInput},
  prelude::*,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenTree {
  Lone(Token),
  Parens(Vec<(TokenTree, SimpleSpan)>),
  Brackets(Vec<(TokenTree, SimpleSpan)>),
  Braces(Vec<(TokenTree, SimpleSpan)>),
  TreeError,
}
// TODO: we can make this type 25% smaller if we replace the Vec with a boxed slice.

pub fn trees_of(
  tokens: &[(Token, SimpleSpan)], file_id: FileID,
) -> Vec<(TokenTree, SimpleSpan)> {
  let eoi: SimpleSpan = match tokens.last() {
    Some(s) => s.1,
    None => return Vec::new(),
  };
  let recovery = via_parser(
    any()
      .repeated()
      .at_least(1)
      .to(TokenTree::TreeError)
      .map_with(|tree, ex| (tree, ex.span())),
  );

  let tree_parser =
    token_tree_p().recover_with(recovery).repeated().collect::<Vec<_>>();

  let (opt_out, errors) = tree_parser
    .parse(Input::map(tokens, eoi, |(tk, span)| (tk, span)))
    .into_output_errors();

  log_error_iter(
    errors
      .into_iter()
      .map(|error| YagError::TokenTreeParseError(file_id, error.into_owned())),
  );

  opt_out.unwrap_or_default()
}

/// Allow you to easily assert an output type for a parser.
///
/// This lets you check that any parser is actually outputting the type that you
/// think it is:
///
/// ```txt
/// // triggers a build error if the output type doesn't match
/// assert_output::<(TokenTree, SimpleSpan), _, _, _>(&p);
/// ```
///
/// This function does not actually run any code, it won't harm the performance
/// of a release build.
fn assert_output<'src, O, P, I, E>(_: &P)
where
  P: Parser<'src, I, O, E>,
  I: Input<'src>,
  E: ParserExtra<'src, I>,
{
}

/// Parses one token tree, and its span.
fn token_tree_p<'src, I>()
-> impl Parser<'src, I, (TokenTree, SimpleSpan), Err<Rich<'src, Token, SimpleSpan>>>
+ Clone
where
  I: BorrowInput<'src, Token = Token, Span = SimpleSpan> + ValueInput<'src>,
{
  recursive(|tokens| {
    // Looks like `{ ... }`
    let braces = tokens
      .clone()
      .repeated()
      .collect()
      .delimited_by(open_brace_p(), close_brace_p())
      .map_with(|out, ex| (TokenTree::Braces(out), ex.span()));

    // Looks like `[ ... ]`
    let brackets = tokens
      .clone()
      .repeated()
      .collect()
      .delimited_by(open_bracket_p(), close_bracket_p())
      .map_with(|out, ex| (TokenTree::Brackets(out), ex.span()));

    // Looks like `( ... )`
    let parens = tokens
      .clone()
      .repeated()
      .collect()
      .delimited_by(open_paren_p(), close_paren_p())
      .map_with(|out, ex| (TokenTree::Parens(out), ex.span()));

    // Looks like something that does *NOT* open or close one of the other
    // types.
    let lone =
      non_tree_token_p().map_with(|out, ex| (TokenTree::Lone(out), ex.span()));

    assert_output::<(TokenTree, SimpleSpan), _, _, _>(&lone);

    // comments get stripped from the output.
    let comment = {
      // Looks like `// ...`
      let single_comment = single_line_comment_p();
      // Looks like `/* ... */`
      let block_comment = tokens
        .clone()
        .repeated()
        .delimited_by(open_comment_p(), close_comment_p())
        .ignored();

      single_comment.or(block_comment)
    };

    let x =
      choice((brackets, braces, parens, lone)).padded_by(comment.repeated());

    x
  })
}

/// Parses an `OpBrace`, which is then discarded.
fn open_brace_p<'src, I>()
-> impl Parser<'src, I, (), Err<Rich<'src, Token, SimpleSpan>>> + Clone
where
  I: BorrowInput<'src, Token = Token, Span = SimpleSpan> + ValueInput<'src>,
{
  select! {
    Token::OpBrace => (),
  }
  .labelled("open_brace")
  .as_context()
}

/// Parses a `ClBrace`, which is then discarded.
fn close_brace_p<'src, I>()
-> impl Parser<'src, I, (), Err<Rich<'src, Token, SimpleSpan>>> + Clone
where
  I: BorrowInput<'src, Token = Token, Span = SimpleSpan> + ValueInput<'src>,
{
  select! {
    Token::ClBrace => (),
  }
  .labelled("close_brace")
  .as_context()
}

/// Parses an `OpBracket`, which is then discarded.
fn open_bracket_p<'src, I>()
-> impl Parser<'src, I, (), Err<Rich<'src, Token, SimpleSpan>>> + Clone
where
  I: BorrowInput<'src, Token = Token, Span = SimpleSpan> + ValueInput<'src>,
{
  select! {
    Token::OpBracket => (),
  }
  .labelled("open_bracket")
  .as_context()
}

/// Parses a `ClBracket`, which is then discarded.
fn close_bracket_p<'src, I>()
-> impl Parser<'src, I, (), Err<Rich<'src, Token, SimpleSpan>>> + Clone
where
  I: BorrowInput<'src, Token = Token, Span = SimpleSpan> + ValueInput<'src>,
{
  select! {
    Token::ClBracket => (),
  }
  .labelled("close_bracket")
  .as_context()
}

/// Parses an `OpParen`, which is then discarded.
fn open_paren_p<'src, I>()
-> impl Parser<'src, I, (), Err<Rich<'src, Token, SimpleSpan>>> + Clone
where
  I: BorrowInput<'src, Token = Token, Span = SimpleSpan> + ValueInput<'src>,
{
  select! {
    Token::OpParen => (),
  }
  .labelled("open_paren")
  .as_context()
}

/// Parses a `ClParen`, which is then discarded.
fn close_paren_p<'src, I>()
-> impl Parser<'src, I, (), Err<Rich<'src, Token, SimpleSpan>>> + Clone
where
  I: BorrowInput<'src, Token = Token, Span = SimpleSpan> + ValueInput<'src>,
{
  select! {
    Token::ClParen => (),
  }
  .labelled("close_paren")
  .as_context()
}

/// Parses an `OpCommentBlock`, which is then discarded.
fn open_comment_p<'src, I>()
-> impl Parser<'src, I, (), Err<Rich<'src, Token, SimpleSpan>>> + Clone
where
  I: BorrowInput<'src, Token = Token, Span = SimpleSpan> + ValueInput<'src>,
{
  select! {
    Token::OpBlockComment => (),
  }
  .labelled("open_comment")
  .as_context()
}

/// Parses a `ClCommentBlock`, which is then discarded.
fn close_comment_p<'src, I>()
-> impl Parser<'src, I, (), Err<Rich<'src, Token, SimpleSpan>>> + Clone
where
  I: BorrowInput<'src, Token = Token, Span = SimpleSpan> + ValueInput<'src>,
{
  select! {
    Token::ClBlockComment => (),
  }
  .labelled("close_comment")
  .as_context()
}

/// Parses a token which isn't a tree opener or closer.
fn non_tree_token_p<'src, I>()
-> impl Parser<'src, I, Token, Err<Rich<'src, Token, SimpleSpan>>> + Clone
where
  I: BorrowInput<'src, Token = Token, Span = SimpleSpan> + ValueInput<'src>,
{
  none_of([
    Token::OpBracket,
    Token::ClBracket,
    Token::OpBrace,
    Token::ClBrace,
    Token::OpParen,
    Token::ClParen,
    Token::OpBlockComment,
    Token::ClBlockComment,
  ])
  .labelled("non_tree_token")
  .as_context()
}

/// Parses an `SingleComment`, which is then discarded.
fn single_line_comment_p<'src, I>()
-> impl Parser<'src, I, (), Err<Rich<'src, Token, SimpleSpan>>> + Clone
where
  I: BorrowInput<'src, Token = Token, Span = SimpleSpan> + ValueInput<'src>,
{
  select! {
    Token::LineComment => (),
  }
  .labelled("single_comment")
  .as_context()
}
