use yagbas::*;

fn fake_file_id(u: usize) -> FileID {
  assert!(u != 0);
  unsafe { core::mem::transmute(u) }
}

#[track_caller]
fn assert_no_parse_errors(s: &'static str) -> Vec<AstItem> {
  let tokens = tokens_of(s);
  let (trees, tree_errors) = trees_of(&tokens);
  for tree_error in tree_errors.iter() {
    eprintln!("{tree_error:?}");
  }
  assert!(tree_errors.is_empty());
  let yag_state = YagParserState { file_id: fake_file_id(1), source: s };
  let (items, item_errors) = items_of(&trees, yag_state);
  for item_error in item_errors.iter() {
    eprintln!("{item_error:?}");
  }
  assert!(item_errors.is_empty());
  items
}

#[test]
fn test_empty_source() {
  assert_no_parse_errors("");
}
#[test]
fn test_empty_fn() {
  assert_no_parse_errors("fn foo() {}");
}
#[test]
fn test_just_comment_fn() {
  assert_no_parse_errors(
    "fn foo() {
      //
    }",
  );
}
#[test]
fn test_let_statements() {
  assert_no_parse_errors(
    "fn foo() {
      let x;
    }",
  );
  assert_no_parse_errors(
    "fn foo() {
      let x: u8;
    }",
  );
  assert_no_parse_errors(
    "fn foo() {
      let x = 1;
    }",
  );
  assert_no_parse_errors(
    "fn foo() {
      let x: u8 = 1;
      let x = if condition {
        3
      } else {
        4
      };
    }",
  );
}
#[test]
fn test_assign_statements() {
  assert_no_parse_errors(
    "fn foo() {
      x = 3;
    }",
  );
}
#[test]
fn test_bin_op_assign_statements() {
  assert_no_parse_errors(
    "fn foo() {
      x += 3;
      x *= 4;
      x /= 5;
      x %= 6;
      x <<= 7;
      x >>= 8;
      x ^= 9;
      x |= 10;
      x &= 11;
    }",
  );
}

#[test]
fn test_if_statements() {
  assert_no_parse_errors(
    "fn foo() {
      if a {}
      if a {} else {}
      if a {};
      if a {} else {};
      if a {
        let x = 1 + 2;
        4
      }
    }",
  );
}

#[test]
fn test_loop_statements() {
  assert_no_parse_errors(
    "fn foo() {
      loop {}
      loop {};
      'named: loop {}
      'named: loop {};
      loop 3 times {}
      loop 3 times {};
      loop count times {}
      loop count times {};
      loop if FOO { 4 } else { 3 } times {}
    }",
  );
}
#[test]
fn test_lib_std_memcpy() {
  assert_no_parse_errors(
    "fn memcpy(dst: ptr, src: ptr, count: u16) {
      loop count times {
        *dst = *src;
        dst += 1;
        src += 1;
      }
    }",
  );
}
#[test]
fn test_setup_interrupts() {
  assert_no_parse_errors(
    "fn setup_vblank_and_stat_interrupts() {
      *STAT = LcdStatus { lyc_eq_ly_irq };
      *LYC = 96;
      *IE = Interrupts { vblank, stat };
      *IF = Interrupts {};
      enable_interrupts!();
    }",
  );
}
#[test]
fn test_wait_for_vblank() {
  assert_no_parse_errors(
    "fn wait_vblank() {
      *VBlankDone = 0;
      loop {
        halt!();
        if *VBlankDone != 0 {
          return
        }
      }
    }",
  );
}
#[test]
fn test_lib_std_get_keys_pressed() {
  assert_no_parse_errors(
    "fn get_keys_pressed() -> KeysPressed {
      // set for button reading, then read twice to ensure
      // the correct data comes in.
      *P1 = 0b1101_1111;
      let buttons_released = *P1;
      let buttons_released = *P1;
      // switch to reading directions, now we read 8 times
      // to allow the register signals to fully update.
      *P1 = 0b1110_1111;
      let dpad_released;
      loop 8 times {
        dpad_released = *P1;
      }
      // release the input reading, which saves some power.
      *P1 = 0b1111_1111;
      buttons_released |= $F0;
      dpad_released |= $F0;
      let all_released = swap!(dpad_released) | buttons_released;
      let all_pressed = all_released ^ -1;
      return KeysPressed(all_pressed)
    }",
  );
}

#[test]
fn test_empty_bitbag() {
  assert_no_parse_errors("bitbag Foo {}");
}
#[test]
fn test_basic_bitbag() {
  assert_no_parse_errors(
    "bitbag ObjAttrs {
      cgb_palette: 0-2,
      cgb_tile_bank: 3,
      dmg_pal_1: 4,
      x_flip: 5,
      y_flip: 6,
      bg_win_priority: 7,
    }",
  );
}

#[test]
fn test_empty_struct() {
  assert_no_parse_errors("struct Foo {}");
}
#[test]
fn test_basic_struct() {
  assert_no_parse_errors(
    "struct Obj {
      y: u8,
      x: u8,
      tile_id: u8,
      attrs: ObjAttrs,
    }",
  );
}

#[test]
fn test_basic_const() {
  assert_no_parse_errors("const TILEMAP_WIDTH: u8 = 32;");
}

#[test]
fn test_basic_static_rom() {
  assert_no_parse_errors(
    "static rom TileData: [Tile; _] = include_png_tiles!(\"starry-night-tiles.png\");",
  );
}
#[test]
fn test_basic_static_ram() {
  assert_no_parse_errors("static ram BallMomentumX: i8 = 1;");
}
#[test]
fn test_basic_static_mmio() {
  assert_no_parse_errors("static mmio LY: u8;");
}
#[test]
fn test_static_with_attribute() {
  assert_no_parse_errors(
    "#[location($FF44)]
    static mmio LY: u8;",
  );
}
