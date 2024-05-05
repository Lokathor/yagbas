use clap::{Args, Parser, Subcommand};

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

  /// Un-build a rom back into source code.
  Unbuild(UnbuildArgs),
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

#[derive(Args, Debug, Clone)]
pub struct UnbuildArgs {
  /// The rom file to un-build back into source code.
  pub file: String,

  /// Output file name (prints to stdout by default)
  #[arg(short, long)]
  pub out: Option<String>,
}

pub fn main() {
  let cli = Cli::parse();
  match cli.command {
    Commands::Build(args) => build(args),
    Commands::Unbuild(args) => unbuild(args),
  }
}

fn build(_args: BuildArgs) {
  println!("todo: implement building")
}

fn unbuild(args: UnbuildArgs) {
  eprintln!("Reading `{f}`...", f = args.file);
  let rom_bytes = match std::fs::read(&args.file) {
    Ok(vec) => vec,
    Err(e) => {
      println!("File Read Error: {e:?}");
      return;
    }
  };
  eprintln!("Got `{b}` bytes.", b = rom_bytes.len());
  if rom_bytes.len() < 0x0150 {
    println!("File is too small to be a GB rom!")
  }

  let title_bytes = rom_bytes.get(0x0134..=0x0143).unwrap();
  let zero = title_bytes.iter().position(|b| *b == b'\0').unwrap_or(title_bytes.len());
  let title = core::str::from_utf8(&title_bytes[..zero]).unwrap_or("NotUtf8!");
  println!("Title: {title}");

  let cart_type = rom_bytes[0x0147];
  println!("Cart Type: {cart_type:02X}");

  let rom_size = rom_bytes[0x0148];
  println!("ROM Size: {rom_size:02X}");

  let ram_size = rom_bytes[0x0149];
  println!("RAM Size: {ram_size:02X}");

  let entry_bytes = rom_bytes.get(0x0100..=0x0103).unwrap();
  let decoded = decode_bytes(entry_bytes);
  println!("Entry Point: {decoded:?}");

  for rst in [0, 8, 0x10, 0x18, 0x20, 0x28, 0x30, 0x38] {
    let rst_bytes = rom_bytes.get(rst..(rst + 8)).unwrap();
    let decoded = decode_bytes(rst_bytes);
    println!("RST(${rst:02X}): {decoded:?}");
  }
  for (irq, name) in
    [(0x40, "VBlank"), (0x48, "LCD"), (0x50, "Timer"), (0x58, "Serial"), (0x60, "Joypad")]
  {
    let irq_bytes = rom_bytes.get(irq..(irq + 8)).unwrap();
    let decoded = decode_bytes(irq_bytes);
    println!("IRQ({name}): {decoded:?}");
  }

  // /*
  let prog_bytes = rom_bytes.get(0x1000..0x1012).unwrap();
  let decoded = decode_bytes(prog_bytes);
  println!("Prog Bytes: {decoded:?}");
  // */
}

#[derive(Clone, Copy)]
pub enum DecodedByte {
  /// unknown raw byte
  Raw(u8),
  /// `di`
  DisableInterrupts,
  /// `jp imm16`
  Jump(u16),
  /// `call imm16`
  Call(u16),
  /// `a = imm8`
  LoadAImm8(u8),
  /// `$FF00+imm8 = a`
  LoadHighA(u8),
  /// `ret`
  Return,
  /// `nop`
  Nop,
  /// `push af`
  PushAF,
  /// `push bc`
  PushBC,
  /// `push de`
  PushDE,
  /// `push hl`
  PushHL,
}
impl core::fmt::Debug for DecodedByte {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      DecodedByte::DisableInterrupts => write!(f, "DisableInterrupts"),
      DecodedByte::Jump(b) => write!(f, "Jump(${b:04X})"),
      DecodedByte::Call(b) => write!(f, "Call(${b:04X})"),
      DecodedByte::Raw(b) => write!(f, "Raw(${b:02X})"),
      DecodedByte::LoadAImm8(b) => write!(f, "LoadAImm8(${b:02X})"),
      DecodedByte::LoadHighA(b) => match b {
        0x00 => write!(f, "LoadHighA(JOYP)"),
        other => write!(f, "LoadHighA($FF00+${other:02X})"),
      },
      DecodedByte::Return => write!(f, "Return"),
      DecodedByte::Nop => write!(f, "Nop"),
      DecodedByte::PushAF => write!(f, "PushAF"),
      DecodedByte::PushBC => write!(f, "PushBC"),
      DecodedByte::PushDE => write!(f, "PushDE"),
      DecodedByte::PushHL => write!(f, "PushHL"),
    }
  }
}

#[allow(clippy::redundant_at_rest_pattern)]
#[allow(unused_variables)]
fn decode_bytes(mut bytes: &[u8]) -> Vec<DecodedByte> {
  let mut out = Vec::new();
  while let Some((head, tail)) = bytes.split_first() {
    match *head {
      0x00 => out.push(DecodedByte::Nop),
      0x3E => match tail {
        [d8, tail2 @ ..] => {
          out.push(DecodedByte::LoadAImm8(*d8));
          bytes = tail2;
          continue;
        }
        [tail2 @ ..] => {
          todo!()
        }
      },
      0xC3 => match tail {
        [j0, j1, tail2 @ ..] => {
          out.push(DecodedByte::Jump(u16::from_le_bytes([*j0, *j1])));
          bytes = tail2;
          continue;
        }
        [j0, tail2 @ ..] => {
          todo!()
        }
        [tail2 @ ..] => {
          todo!()
        }
      },
      0xC5 => out.push(DecodedByte::PushBC),
      0xCD => match tail {
        [j0, j1, tail2 @ ..] => {
          out.push(DecodedByte::Call(u16::from_le_bytes([*j0, *j1])));
          bytes = tail2;
          continue;
        }
        [j0, tail2 @ ..] => {
          todo!()
        }
        [tail2 @ ..] => {
          todo!()
        }
      },
      0xD5 => out.push(DecodedByte::PushDE),
      0xE5 => out.push(DecodedByte::PushHL),
      0xC9 => out.push(DecodedByte::Return),
      0xE0 => match tail {
        [a8, tail2 @ ..] => {
          out.push(DecodedByte::LoadHighA(*a8));
          bytes = tail2;
          continue;
        }
        [tail2 @ ..] => {
          todo!()
        }
      },
      0xF3 => out.push(DecodedByte::DisableInterrupts),
      0xF5 => out.push(DecodedByte::PushAF),
      other => out.push(DecodedByte::Raw(other)),
    }
    bytes = tail;
  }
  out
}
