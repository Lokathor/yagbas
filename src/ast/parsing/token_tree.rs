use super::*;

/// Parses [Token] into [TokenTree].
pub fn token_tree_p<'src, I>(
) -> impl Parser<'src, I, FileSpanned<TokenTree>, ErrRichToken<'src>> + Clone
where
  I: BorrowInput<'src, Token = Token, Span = FileSpan> + ValueInput<'src>,
{
  recursive(|tt| {
    let base = tt.map_with(FileSpanned::from_extras).repeated();

    // Looks like `{ ... }`
    let braces = base
      .clone()
      .collect()
      .delimited_by(open_brace_p(), close_brace_p())
      .map(TokenTree::Braces)
      .labelled("braces_group")
      .as_context();

    // Looks like `[ ... ]`
    let brackets = base
      .clone()
      .collect()
      .delimited_by(open_bracket_p(), close_bracket_p())
      .map(TokenTree::Brackets)
      .labelled("brackets_group")
      .as_context();

    // Looks like `( ... )`
    let parens = base
      .clone()
      .collect()
      .delimited_by(open_paren_p(), close_paren_p())
      .map(TokenTree::Parens)
      .labelled("parens_group")
      .as_context();

    // Looks like something that does *NOT* open or close one of the other
    // types.
    let single = none_of([
      OpBracket,
      ClBracket,
      OpBrace,
      ClBrace,
      OpParen,
      ClParen,
      CommentBlockStart,
      CommentBlockEnd,
    ])
    .map(TokenTree::Lone)
    .labelled("lone_token")
    .as_context();

    // comments get stripped from the output.
    let comment = {
      // Looks like `//`
      let single_comment = select! {
        CommentSingle => (),
      };
      // Looks like `/* ... */`
      let comment_block_start = select! {
        CommentBlockStart => (),
      }
      .labelled("comment_block_start")
      .as_context();
      let comment_block_end = select! {
        CommentBlockEnd => (),
      }
      .labelled("comment_block_end")
      .as_context();
      let block_comment = base
        .clone()
        .delimited_by(comment_block_start, comment_block_end)
        .ignored()
        .labelled("block_comment")
        .as_context();

      single_comment.or(block_comment)
    };

    let x =
      choice((brackets, braces, parens, single)).padded_by(comment.repeated());

    x
  })
  .map_with(FileSpanned::from_extras)
  .labelled("token_tree")
  .as_context()
}

/// Parses an `OpBrace`, which is then discarded.
pub fn open_brace_p<'src, I>(
) -> impl Parser<'src, I, (), ErrRichToken<'src>> + Clone
where
  I: BorrowInput<'src, Token = Token, Span = FileSpan> + ValueInput<'src>,
{
  select! {
    OpBrace => (),
  }
  .labelled("open_brace")
  .as_context()
}
/// Parses a `ClBrace`, which is then discarded.
pub fn close_brace_p<'src, I>(
) -> impl Parser<'src, I, (), ErrRichToken<'src>> + Clone
where
  I: BorrowInput<'src, Token = Token, Span = FileSpan> + ValueInput<'src>,
{
  select! {
    ClBrace => (),
  }
  .labelled("close_brace")
  .as_context()
}

/// Parses an `OpBracket`, which is then discarded.
pub fn open_bracket_p<'src, I>(
) -> impl Parser<'src, I, (), ErrRichToken<'src>> + Clone
where
  I: BorrowInput<'src, Token = Token, Span = FileSpan> + ValueInput<'src>,
{
  select! {
    OpBracket => (),
  }
  .labelled("open_bracket")
  .as_context()
}
/// Parses a `ClBracket`, which is then discarded.
pub fn close_bracket_p<'src, I>(
) -> impl Parser<'src, I, (), ErrRichToken<'src>> + Clone
where
  I: BorrowInput<'src, Token = Token, Span = FileSpan> + ValueInput<'src>,
{
  select! {
    ClBracket => (),
  }
  .labelled("close_bracket")
  .as_context()
}

/// Parses an `OpParen`, which is then discarded.
pub fn open_paren_p<'src, I>(
) -> impl Parser<'src, I, (), ErrRichToken<'src>> + Clone
where
  I: BorrowInput<'src, Token = Token, Span = FileSpan> + ValueInput<'src>,
{
  select! {
    OpParen => (),
  }
  .labelled("open_paren")
  .as_context()
}
/// Parses a `ClParen`, which is then discarded.
pub fn close_paren_p<'src, I>(
) -> impl Parser<'src, I, (), ErrRichToken<'src>> + Clone
where
  I: BorrowInput<'src, Token = Token, Span = FileSpan> + ValueInput<'src>,
{
  select! {
    ClParen => (),
  }
  .labelled("close_paren")
  .as_context()
}
