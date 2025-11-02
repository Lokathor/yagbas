
SECTION "graphics", ROM0
graphicTiles:
  ; Graphics data, below contains 3x the same 8x8 graphics tile in different formats.
  ; Prefered way would be to use INCBIN with rgbgfx, which is currently not possible in rgbds-live.
opt g.123
  dw `.111111.
  dw `1......1
  dw `1.3..3.1
  dw `1......1
  dw `1......1
  dw `1.2..2.1
  dw `1..22..1
  dw `.111111.
  
  dw $007e
  dw $0081
  dw $24a5
  dw $0081
  dw $0081
  dw $2481
  dw $1881
  dw $007e
  
  db $7e, $00, $81, $00, $a5, $24, $81, $00, $81, $00, $81, $24, $81, $18, $7e, $00
.end:


fn main() {
  disable_lcd()
  load_tiles()
  load_palette()
  enable_lcd()
  loop {}
}

fn disable_lcd() {
  loop {
    if [LY] == VBLANK_START {
      break
    }
  }
}

fn load_palette() {
  [BGP] = STANDARD_PALETTE
}

fn load_tiles() {
  memcpy($8000, GraphicTiles, size_of!(GraphicTiles))
}

fn enable_lcd() {
  [LCDC] = LcdCtrl { bg_win_enabled, lcd_enabled }
}
