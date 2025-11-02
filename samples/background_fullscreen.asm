
#[hram]
static mut VBlankDone = 0;

fn vblank_vector() {
  [VBlankDone] = 1
  [LCDC] = LcdCtrl {
    lcd_enabled, bg_win_enabled, 
  }
}

fn stat_vector() {
  [LCDC] = LcdCtrl {
    lcd_enabled, bg_win_enabled, bg_win_tiles_signed,
  }
}

fn main() {
  disable_interrupts!()
  set_stack_pointer!($E000)
  [NR52] = AudioMain {}
  'wait_for_vblank: loop {
    if [LY] == VBLANK_START {
      break
    }
  }
  [LCDC] = LcdCtrl {}
  // copy just 240 tiles
  memcpy($8000, TileData, size_of!(TileData.0)*240)
  // copy the other 120 tiles
  memcpy($9000, TileData.241, size_of!(TileData.0)*120)
  
  let tiles_per_row_advance = TILEMAP_WIDTH-SCREEN_WIDTH_TILES
  let tile_addr = $9800
  let id = 0
  let y = SCREEN_HEIGHT_TILES
  let x = SCREEN_WIDTH_TILES
  loop {
    loop x times {
      [tile_addr] = id
      tile_addr += 1
      id += 1
    }
    tile_addr += tiles_per_row_advance
    y -= 1
    if y == 7 {
      id = 0
    }
    if y == 0 {
      break
    }
  }
  
  [BGP] = STANDARD_PALETTE
  [SCX] = 0
  [SCY] = 0
  
  // todo: setup stat and vblank interrupts
  
  [LCDC] = LdcCtrl { lcd_enabled, bg_win_enabled }
  loop {
    wait_vblank()
  }
}

    ; Setwrup the VBlank and STAT interrupts
    ld a, STAT_LYC      ; Load the flag to enable LYC STAT interrupts into A
    ldh [rSTAT], a      ; Load the prepared flag into rSTAT to enable the LY=LYC interrupt source 
    ld a, 96            ; Set which line to trigger the LY=LYC interrupt on by setting the rLYC register
    ldh [rLYC], a       ;  ...
    ld a, IE_VBLANK | IE_STAT ; Load the flag to enable the VBlank and STAT interrupts into A
    ldh [rIE], a        ; Load the prepared flag into the interrupt enable register
    xor a               ; Set A to zero
    ldh [rIF], a        ; Clear any lingering flags from the interrupt flag register to avoid false interrupts
    ei                  ; enable interrupts!

fm wait_vblank() {
  [VBlankDone] = 0
  loop {
    halt!()
    if [VBlankDone] != 0 {
      return
    }
  }
}

SECTION "Tile Data", ROMX
TileData:
    incbin "starry-night-tiles.2bpp" ; Include binary tile data inline using incbin
