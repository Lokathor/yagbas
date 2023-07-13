use chumsky::span::SimpleSpan;

pub struct Ast<T> {
  pub items: Vec<(T, SimpleSpan)>,
}
