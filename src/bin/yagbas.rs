use clap::{Args, Parser, Subcommand};
use logos::Logos;
use yagbas::lexer::Token;

#[test]
fn verify_cli() {
  use clap::CommandFactory;
  Cli::command().debug_assert()
}

#[derive(Parser, Debug, Clone)]
#[command(version, about)]
pub struct Cli {
  #[command(subcommand)]
  command: Commands,
}

#[derive(Subcommand, Debug, Clone)]
pub enum Commands {
  /// Build source files into a rom.
  Build(BuildArgs),
}

#[derive(Args, Debug, Clone)]
pub struct BuildArgs {
  /// One or more source files.
  pub files: Vec<String>,

  /// Output file name. Defaults to the name of the first src file with `.gb`
  /// extension
  #[arg(short, long)]
  pub out: Option<String>,
}

pub fn main() {
  let cli = Cli::parse();
  match cli.command {
    Commands::Build(args) => build(args),
  }
}

fn build(args: BuildArgs) {
  println!("args: {args:?}");
  for file in &args.files {
    println!("== `{file}`");
    let string = std::fs::read_to_string(file).unwrap();
    let tokens: Vec<Token> = Token::lexer(&string).map(|r| r.unwrap()).collect();
    println!("{tokens:?}");
  }
}
