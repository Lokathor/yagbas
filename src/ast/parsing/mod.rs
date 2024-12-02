use chumsky::{
  error::Rich,
  extra::{Err, ParserExtra},
  input::{BorrowInput, Input, MapExtra, MappedInput, ValueInput},
  prelude::*,
  Parser,
};

use super::*;

pub mod const_;
pub mod expression;
pub mod function;
pub mod item;
pub mod lone_tokens;
pub mod statement;
pub mod static_;
pub mod token_tree;

pub use const_::*;
pub use expression::*;
pub use function::*;
pub use item::*;
pub use lone_tokens::*;
pub use statement::*;
pub use static_::*;
pub use token_tree::*;

pub type ErrRichToken<'src> = Err<Rich<'src, Token, FileSpan>>;
pub type ErrRichTokenTree<'src> = Err<Rich<'src, TokenTree, FileSpan>>;

/// Lex the textual content of a program module into tokens.
#[inline]
pub fn lex_module_text(
  text: &str, id: SrcID, err_bucket: &mut Vec<YagError>,
) -> Vec<FileSpanned<Token>> {
  let mut output = Vec::new();
  for (res_token, range_span) in Token::lexer(text).spanned() {
    let file_span = FileSpan::new(id, range_span);
    match res_token {
      Ok(token) => {
        output.push(FileSpanned::new(token, file_span));
      }
      Err(()) => {
        output.push(FileSpanned::new(Token::TokenError, file_span));
        err_bucket.push(YagError::Tokenization(file_span));
      }
    }
  }
  output
}

/// Converts a slice of tokens into a vec of token trees.
///
/// The token tree creation step has *exceptionally* bad error recovery.
pub fn parse_token_trees(
  tokens: &[FileSpanned<Token>], err_bucket: &mut Vec<YagError>,
) -> Vec<FileSpanned<TokenTree>> {
  let end_span = match tokens.last() {
    None => return Vec::new(),
    Some(t) => FileSpan {
      id: t._span.id,
      // Note(Lokathor): Yes, we're using `end` twice deliberately. Chumsky is
      // a silly friend, this is just how it works.
      start: t._span.end,
      end: t._span.end,
    },
  };
  let recovery = via_parser(
    any()
      .repeated()
      .at_least(1)
      .to(TokenTree::TreeError)
      .map_with(FileSpanned::from_extras),
  );
  let tree_parser = token_tree_p().recover_with(recovery).repeated().collect();
  // Note(Lokathor): This looks *close* to what `make_tt_input` does, but this
  // goes from Tokens to TokenTrees, instead of from TokenTrees to something
  // else, so we can't use `make_tt_input` here.
  let (opt_output, errors) = tree_parser
    .parse(Input::map(tokens, end_span, |fs| (&fs._payload, &fs._span)))
    .into_output_errors();
  let trees: Vec<FileSpanned<TokenTree>> = opt_output.unwrap_or_default();
  err_bucket
    .extend(errors.into_iter().map(|e| YagError::TokenTree(e.into_owned())));
  trees
}

pub fn parse_items(
  trees: &[FileSpanned<TokenTree>], err_bucket: &mut Vec<YagError>,
) -> Vec<FileSpanned<Item>> {
  let end_span = match trees.last() {
    None => return Vec::new(),
    Some(t) => FileSpan {
      id: t._span.id,
      // Note(Lokathor): Yes, we're using `end` twice deliberately. Chumsky is
      // a silly friend, this is just how it works.
      start: t._span.end,
      end: t._span.end,
    },
  };
  let strategy = via_parser(
    item_sep_p()
      .then(any().and_is(item_sep_p().not()).repeated())
      .map_with(|_, extras| FileSpanned::from_extras(Item::ItemError, extras)),
  );
  let items_parser = item_p(make_tt_input)
    .padded_by(newline_p().repeated())
    .recover_with(strategy)
    .repeated()
    .collect::<Vec<FileSpanned<Item>>>();
  let (opt_output, item_errors_unowned) =
    items_parser.parse(make_tt_input(trees, end_span)).into_output_errors();
  let items: Vec<FileSpanned<Item>> = opt_output.unwrap_or_default();
  err_bucket.extend(
    item_errors_unowned.into_iter().map(|e| YagError::Item(e.into_owned())),
  );
  items
}

/// Makes a token tree slice into an [Input](chumsky::input::Input).
///
/// This is used by all our parsers that need to look at the content of a token
/// tree group.
///
/// * When making a parser that looks at the content of a token tree group, we
///   need to run a parser on that inner content.
/// * Running a parser on the content of a tree group uses `Input::map` to turn
///   our custom `FileSpanned<T>` type (which chumsky doesn't know about) into
///   `(T, FileSpan)` (which is what chumsky is expecting).
/// * If any part of such a parser is recursive (eg, statements, expressions,
///   etc) then we'd get a type error because Rust doesn't see two calls to
///   `Input::map` with different closures as being the same type just because
///   the two closures have the exact same expression.
/// * So we force all our calls to any parser that uses grouped content to go
///   through this one specific function, which gives them all the exact same
///   mapping closure, which lets Rust see that they're all the same type.
#[allow(clippy::needless_lifetimes)]
pub fn make_tt_input<'src>(
  trees: &'src [FileSpanned<TokenTree>], eoi: FileSpan,
) -> impl BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>
{
  Input::map(trees, eoi, |fsd| (&fsd._payload, &fsd._span))
}

/// Lets you `select_ref!` the content out of some `Braces`
pub fn braces_content_p<'src, I, M>(
  make_input: M,
) -> impl Parser<'src, I, I, ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
  M: Fn(&'src [FileSpanned<TokenTree>], FileSpan) -> I + Copy + 'src,
{
  select_ref! {
    Braces(b) = ex => make_input(b, ex.span()),
  }
}

/// Lets you `select_ref!` the content out of some `Brackets`
pub fn brackets_content_p<'src, I, M>(
  make_input: M,
) -> impl Parser<'src, I, I, ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
  M: Fn(&'src [FileSpanned<TokenTree>], FileSpan) -> I + Copy + 'src,
{
  select_ref! {
    Brackets(b) = ex => make_input(b, ex.span()),
  }
}

/// Lets you `select_ref!` the content out of some `Parens`
pub fn parens_content_p<'src, I, M>(
  make_input: M,
) -> impl Parser<'src, I, I, ErrRichTokenTree<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = FileSpan> + ValueInput<'src>,
  M: Fn(&'src [FileSpanned<TokenTree>], FileSpan) -> I + Copy + 'src,
{
  select_ref! {
    Parens(b) = ex => make_input(b, ex.span()),
  }
}
