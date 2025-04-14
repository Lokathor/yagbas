use crate::{Token, tokens_of};
use chumsky::{extra::Full, prelude::*};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenTree {
  Lone(Token),
  Parens(Vec<(TokenTree, SimpleSpan)>),
  Brackets(Vec<(TokenTree, SimpleSpan)>),
  Braces(Vec<(TokenTree, SimpleSpan)>),
  TreeError,
}

pub fn trees_of(source: &str) -> Vec<(TokenTree, SimpleSpan)> {
  let tokens: Vec<(Token, SimpleSpan)> = tokens_of(source);

  let tree_parser = recursive(|tokens| {
    let op_brace = select! {
      Token::OpBrace => (),
    };
    let cl_brace = select! {
      Token::ClBrace => (),
    };
    let op_paren = select! {
      Token::OpParen => (),
    };
    let cl_paren = select! {
      Token::ClParen => (),
    };
    let op_bracket = select! {
      Token::OpBracket => (),
    };
    let cl_bracket = select! {
      Token::ClBracket => (),
    };

    // Looks like `{ ... }`
    let braces = tokens
      .clone()
      .repeated()
      .collect()
      .delimited_by(op_brace, cl_brace)
      .map_with(|out, ex| (TokenTree::Braces(out), ex.span()));

    // Looks like `[ ... ]`
    let brackets = tokens
      .clone()
      .repeated()
      .collect()
      .delimited_by(op_bracket, cl_bracket)
      .map_with(|out, ex| (TokenTree::Brackets(out), ex.span()));

    // Looks like `( ... )`
    let parens = tokens
      .clone()
      .repeated()
      .collect()
      .delimited_by(op_paren, cl_paren)
      .map_with(|out, ex| (TokenTree::Parens(out), ex.span()));

    // Looks like something that does *NOT* open or close one of the other
    // types.
    let single = none_of([
      Token::OpBracket,
      Token::ClBracket,
      Token::OpBrace,
      Token::ClBrace,
      Token::OpParen,
      Token::ClParen,
      Token::OpCommentBlock,
      Token::ClCommentBlock,
    ])
    .map(TokenTree::Lone);

    // comments get stripped from the output.
    let comment = {
      // Looks like `// ...`
      let single_comment = select! {
        Token::SingleComment => (),
      };
      // Looks like `/* ... */`
      let op_comment_block = select! {
        Token::OpCommentBlock => (),
      };
      let cl_comment_block = select! {
        Token::ClCommentBlock => (),
      };
      let block_comment = tokens
        .clone()
        .delimited_by(op_comment_block, cl_comment_block)
        .ignored();

      single_comment.or(block_comment)
    };

    let x =
      choice((brackets, braces, parens, single)).padded_by(comment.repeated());

    x
  })
  .repeated()
  .collect();

  // TODO: handle errors somehow, later on.
  tree_parser.parse(&tokens[..]).into_output().unwrap_or_default()
}
