use super::*;

/// Parses [Token] into [TokenTree].
pub fn token_tree_p<'src, I>(
) -> impl Parser<'src, I, TokenTree, ErrRichToken<'src>> + Clone
where
  I: BorrowInput<'src, Token = Token, Span = FileSpan> + ValueInput<'src>,
{
  recursive(|tt| {
    let base = tt.map_with(|tts, e| FileSpanned::new(tts, e.span())).repeated();

    // Looks like `{ ... }`
    let braces = {
      let open_brace = select! {
        OpBrace => (),
      }
      .labelled("open_brace")
      .as_context();
      let close_brace = select! {
        ClBrace => (),
      }
      .labelled("close_brace")
      .as_context();
      base
        .clone()
        .collect()
        .delimited_by(open_brace, close_brace)
        .map(TokenTree::Braces)
        .labelled("braces_group")
        .as_context()
    };

    // Looks like `[ ... ]`
    let brackets = {
      let open_bracket = select! {
        OpBracket => (),
      }
      .labelled("open_bracket")
      .as_context();
      let close_bracket = select! {
        ClBracket => (),
      }
      .labelled("close_bracket")
      .as_context();
      base
        .clone()
        .collect()
        .delimited_by(open_bracket, close_bracket)
        .map(TokenTree::Brackets)
        .labelled("brackets_group")
        .as_context()
    };

    // Looks like `( ... )`
    let parens = {
      let open_paren = select! {
        OpParen => (),
      }
      .labelled("open_paren")
      .as_context();
      let close_paren = select! {
        ClParen => (),
      }
      .labelled("close_paren")
      .as_context();
      base
        .clone()
        .collect()
        .delimited_by(open_paren, close_paren)
        .map(TokenTree::Parens)
        .labelled("parens_group")
        .as_context()
    };

    // Looks like something that does *NOT* open or close one of the other
    // types.
    let single =
      none_of([OpBracket, ClBracket, OpBrace, ClBrace, OpParen, ClParen])
        .map(TokenTree::Lone);

    // comments get stripped from the output.
    let comment = {
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
      let block_comment = base
        .clone()
        .delimited_by(block_start, block_end)
        .ignored()
        .labelled("block_comment")
        .as_context();

      single_comment.or(block_comment)
    };

    let x = choice((brackets, braces, parens, single))
      .padded_by(comment.repeated())
      .labelled("token_tree")
      .as_context();

    x
  })
}
