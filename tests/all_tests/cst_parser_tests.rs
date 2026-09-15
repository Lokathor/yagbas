use yagbas::cst::{Cst, CstKind, actions::group_module, parser::CstParser};

#[track_caller]
fn cst_no_errors(src: &str) -> Cst {
  let mut p = CstParser::new(src);
  group_module(&mut p);
  let (cst, errors) = p.build_tree();
  assert!(errors.is_empty(), "Cst Parse Errors: {errors:?}");
  let exact_src = cst.to_source_code().unwrap();
  assert_eq!(src, exact_src);
  cst
}

#[test]
fn test_empty_module() {
  cst_no_errors("");
  cst_no_errors(" ");
  cst_no_errors("/**/");
}

#[test]
fn test_const() {
  cst_no_errors("const NAME: Type = expression;");
  cst_no_errors(
    "const VBLANK_START: u8 = 144;
    const BYTES_PER_TILE: u16 = 8;
    const TILES_PER_BLOCK: u16 = 128;
    const TILES_PER_ROW: u16 = 32;
    const EIGHT_ROWS_OF_TILES: u16 = TILES_PER_ROW * 8;",
  );
}

#[test]
fn test_static_mmio() {
  cst_no_errors("static mmio($FF40) LCDC: LcdControl;");
  cst_no_errors(
    "/// vram blocks don't have clear natural names, so we name them
    /// after the base address value.
    static mmio($8800) VRAM_BLOCK_8000: [[u8;BYTES_PER_TILE]; TILES_PER_BLOCK];
    
    /// tilemaps also don't have good sensible names.
    static mmio($9800) TILEMAP_9800: [u8; 32*32];
    static mmio($FF26) AUDIO_MAIN: u8;
    static mmio($FF40) LCDC: u8;
    static mmio($FF44) LY: u8;
    static mmio($FF47) BGP: u8;",
  );
}

#[test]
fn test_static_ram() {
  cst_no_errors("static ram SCORE: u8 = 0;");
}

#[test]
fn test_static_rom() {
  cst_no_errors("static rom BASE_HP: u8 = 12;");
}

#[test]
fn test_function() {
  cst_no_errors("fn foo ( ) { }");
  cst_no_errors(
    "fn add_two(x: u8) -> u8 {
      x + 2
    }",
  );
  cst_no_errors(
    "fn main() {
      // disable sound
      *AUDIO_MAIN = 0;
      // wait for vblank and disable lcd
      loop {
        if *LY == VBLANK_START {
          break
        }
      }
    }",
  );
  cst_no_errors(
    "fn main() {
      let tile = &VRAM_BLOCK_8000[0][0];
      let pattern = $AA;
      for _ in 0..BYTES_PER_TILE {
        *tile = pattern;
        *tile += 1;
      }
    }",
  );
}

#[test]
fn test_struct() {
  cst_no_errors(
    "struct OamData {
      y: u8,
      x: u8,
      tile_index: u8,
      attributes: OamAttrs,
    }",
  );
}

#[test]
fn test_bitbag() {
  cst_no_errors(
    "bitbag IrqFlags {
      vblank: 0,
      lcd: 1,
      timer: 2,
      serial: 3,
      joypad: 4,
    }",
  );
}

#[test]
fn test_use() {
  cst_no_errors("use core::memcpy;");
}

#[test]
fn test_impl() {
  cst_no_errors(
    "impl OamData {
      fn hide(&mut self) {
        self.y = 0;
      }
    }",
  );
}
