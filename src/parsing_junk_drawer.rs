use super::*;
use crate::Token::*;
use crate::TokenTree::*;

pub fn kw_if_p<'src>() -> impl YagParser<'src, ()> {
  select! {
    Lone(KwIf) => ()
  }
}
