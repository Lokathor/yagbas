use crate::{Token, tokens_of};
use chumsky::{
  extra::Err,
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

// TODO: track errors
pub fn trees_of(source: &str) -> Vec<(TokenTree, SimpleSpan)> {
  let tokens: Vec<(Token, SimpleSpan)> = tokens_of(source);
  let eoi: SimpleSpan = match tokens.last() {
    Some(s) => s.1,
    None => return Vec::new(),
  };

  let tree_parser = recursive(|tokens| {
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
    let lone = non_tree_token_p().map(TokenTree::Lone);

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
  .repeated()
  .collect();

  // TODO: handle errors somehow, later on.
  tree_parser
    .parse(Input::map(&tokens[..], eoi, |(tk, span)| (tk, span)))
    .into_output()
    .unwrap_or_default()
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
    Token::OpCommentBlock => (),
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
    Token::ClCommentBlock => (),
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
    Token::OpCommentBlock,
    Token::ClCommentBlock,
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
    Token::SingleComment => (),
  }
  .labelled("single_comment")
  .as_context()
}
