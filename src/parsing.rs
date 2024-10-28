use crate::{
  item::{Function, Item},
  src_files::{FileSpan, FileSpanned},
  statement::Statement,
  str_id::StrID,
  token::Token::{self, *},
  token_tree::TokenTree::{self, *},
};
use chumsky::{
  extra::Err,
  input::{BorrowInput, ValueInput},
  prelude::*,
};

pub type ErrRichToken<'src> = Err<Rich<'src, Token, FileSpan>>;
pub type ErrRichTokenTree<'src> = Err<Rich<'src, TokenTree, FileSpan>>;

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
      let open_bracket = select! {
        OpBrace => (),
      };
      let close_bracket = select! {
        ClBrace => (),
      };
      base
        .clone()
        .collect()
        .delimited_by(open_bracket, close_bracket)
        .map(TokenTree::Braces)
    };

    // Looks like `[ ... ]`
    let brackets = {
      let open_bracket = select! {
        OpBracket => (),
      };
      let close_bracket = select! {
        ClBracket => (),
      };
      base
        .clone()
        .collect()
        .delimited_by(open_bracket, close_bracket)
        .map(TokenTree::Brackets)
    };

    // Looks like `( ... )`
    let parens = {
      let open_paren = select! {
        OpParen => (),
      };
      let close_paren = select! {
        ClParen => (),
      };
      base
        .clone()
        .collect()
        .delimited_by(open_paren, close_paren)
        .map(TokenTree::Parens)
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
      let block_comment =
        base.clone().delimited_by(block_start, block_end).ignored();

      single_comment.or(block_comment)
    };

    let x =
      choice((brackets, braces, parens, single)).padded_by(comment.repeated());

    x
  })
}

/// Parses [TokenTree] into any [Item]
pub fn item_p<'src, I, M>(
  make_input: M,
) -> impl Parser<'src, I, Item, ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
  M: Fn(&'src [FileSpanned<TokenTree>], FileSpan) -> I + Clone + 'src,
{
  let function = function_p(make_input).map(Item::Function);

  let x = choice((function,)).labelled("item").as_context();

  x
}

/// Parses [TokenTree] into specifically a [Function]
pub fn function_p<'src, I, M>(
  make_input: M,
) -> impl Parser<'src, I, Function, ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
  M: Fn(&'src [FileSpanned<TokenTree>], FileSpan) -> I + Clone + 'src,
{
  let name = ident_p()
    .map_with(|i, e| FileSpanned::new(i, e.span()))
    .labelled("fn_name")
    .as_context();
  let args = parenthesis_p().labelled("fn_args").as_context();
  let fn_body = statement_p()
    .map_with(|s: Statement, e| FileSpanned::new(s, e.span()))
    .separated_by(statement_sep_p().repeated().at_least(1))
    .allow_leading()
    .allow_trailing()
    .collect()
    .nested_in(select_ref! {
      Braces(b) = ex => make_input(b, ex.span()),
    })
    .labelled("fn_body")
    .as_context();
  let x = kw_fn_p()
    .ignore_then(name)
    .then(args)
    .then(fn_body)
    .map(|((name, arguments), statements)| Function {
      name,
      arguments,
      statements,
    })
    .labelled("function")
    .as_context();

  x
}

/// Parses [TokenTree] into specifically a [Statement]
pub fn statement_p<'src, I>(
) -> impl Parser<'src, I, Statement, ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
{
  let call = ident_p()
    .then(parenthesis_p())
    .map(|(target, args)| Statement::Call { target, args });

  // TODO: loop support, but that makes the parser recursive.

  let x = choice((kw_return_p(), call));

  x
}

/// Parses a `Lone(Newline)`, which is then discarded.
pub fn newline_p<'src, I>(
) -> impl Parser<'src, I, (), ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
{
  select! {
    Lone(Newline) => (),
  }
}

/// Parses a `Lone(Newline)` or `Lone(Semicolon)`, which is then discarded.
pub fn statement_sep_p<'src, I>(
) -> impl Parser<'src, I, (), ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
{
  select! {
    Lone(Newline) => (),
    Lone(Semicolon) => (),
  }
}

/// Parses a `Lone(KwFn)`, which is then discarded.
pub fn kw_fn_p<'src, I>(
) -> impl Parser<'src, I, (), ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
{
  select! {
    Lone(KwFn) => (),
  }
}

/// Parses a `Lone(KwReturn)` and returns `Statement::Return` instead.
pub fn kw_return_p<'src, I>(
) -> impl Parser<'src, I, Statement, ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
{
  select! {
    Lone(KwReturn) => Statement::Return,
  }
}

/// Parses `Lone(Ident(i))` and returns `i`.
pub fn ident_p<'src, I>(
) -> impl Parser<'src, I, StrID, ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
{
  select! {
    Lone(Ident(i)) => i,
  }
}

/// Parses `Lone(Ident(i))` and returns `i`.
pub fn parenthesis_p<'src, I>(
) -> impl Parser<'src, I, Vec<FileSpanned<TokenTree>>, ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
{
  select! {
    Parens(p) = ex => p,
  }
}
