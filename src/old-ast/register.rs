use super::*;

/// The registers that can be named anywhere in any expression.
///
/// This type isn't strictly necessary, we could use [StrID] in all places where
/// there's a Register value, but it's a little faster to sort out registers
/// from identifiers ahead of time.
#[derive(Debug, Clone, Copy, Display)]
pub enum Register {
  A,
  B,
  C,
  D,
  E,
  H,
  L,
  AF,
  BC,
  DE,
  HL,
  SP,
}

pub fn register_p<'src, I>()
-> impl Parser<'src, I, Register, AstExtras<'src>> + Clone
where
  I: BorrowInput<'src, Token = TokenTree, Span = SimpleSpan> + ValueInput<'src>,
{
  select! {
    TokenTree::Lone(Token::KwA) => { Register::A },
    TokenTree::Lone(Token::KwB) => { Register::B },
    TokenTree::Lone(Token::KwC) => { Register::C },
    TokenTree::Lone(Token::KwD) => { Register::D },
    TokenTree::Lone(Token::KwE) => { Register::E },
    TokenTree::Lone(Token::KwH) => { Register::H },
    TokenTree::Lone(Token::KwL) => { Register::L },
    TokenTree::Lone(Token::KwAF) => { Register::AF },
    TokenTree::Lone(Token::KwBC) => { Register::BC },
    TokenTree::Lone(Token::KwDE) => { Register::DE },
    TokenTree::Lone(Token::KwHL) => { Register::HL },
    TokenTree::Lone(Token::KwSP) => { Register::SP },
  }
}
