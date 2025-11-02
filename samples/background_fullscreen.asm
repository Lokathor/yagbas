
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
}


    ; Copy the first 240 tiles to VRAM starting at $8000
    ld hl, TileData     ; Load the source address of our tiles into HL
    ld de, STARTOF(VRAM); Load the destination address in VRAM into DE
    ld bc, $10*240      ; Load the number of bytes to copy into BC (16 bytes per tile)
    call MemCopy        ; Call our general-purpose memory copy routine

    ; Copy the remaining 120 tiles to VRAM starting at $9000
                        ; Note: HL is already pointing at the start of the data we want to copy
    ld de, STARTOF(VRAM)+$1000  ; Load the destination address in VRAM into DE
    ld bc, $10*120      ; Load the number of bytes to copy into BC (16 bytes per tile)
    call MemCopy        ; Call our general-purpose memory copy routine

    ; Create a tilemap which includes all 360 tiles, in order, starting at tile 0
    ld hl, TILEMAP0     ; Point HL to the first byte of the tilemap ($9800)
    ld de, TILEMAP_WIDTH - SCREEN_WIDTH ; Load the number of tiles to skip per row (32-20) into DE
    xor a               ; Store the tile ID we're writing in A, starting with zero
    ld c, SCREEN_HEIGHT ; Load the total number of rows (18) into C
.yLoop
    ld b, SCREEN_WIDTH  ; Load the number of tiles per row (20) into B
.xLoop
    ld [hli], a         ; Load the current tile ID into HL and increment HL
    inc a               ; Increment the tile ID in A
    dec b               ; Decrement the X counter
    jr nz, .xLoop       ; Loop until we've written a full row
    add hl, de          ; Add the offset to advance to the next row in VRAM to HL
    ld b, a             ; Move the tile ID (A) to B temporarily to free A up
    ld a, c             ; Load the rows remaining into A
    cp 7                ; Compare A to 7 to see if we only have 6 rows remaining
                        ;  (we compare to 7 instead of 6 since we haven't decremented C yet)
    jr nz, .dontReset   ; If we aren't at the "6 rows remaining" point don't reset the tile ID
    ld b, 0             ; Load zero into B as the starting tile ID for the bottom portion
.dontReset
    ld a, b             ; Recover the tile ID from B (either the old one or the reset one) into A
    dec c               ; Decrement the row counter
    jr nz, .yLoop       ; Loop until we've written the top set of tile IDs

    ; Setup palettes and scrolling
    ld a, %11100100     ; Define a 4-shade palette from darkest (11) to lightest (00)
    ldh [rBGP], a       ; Set the background palette

    ld a, 0             ; Load zero into the register A
    ldh [rSCX], a       ; Set the background scroll registers to show the top-left
    ldh [rSCY], a       ;  corner of the background in the top-left corner of the screen

    ; Setup the VBlank and STAT interrupts
    ld a, STAT_LYC      ; Load the flag to enable LYC STAT interrupts into A
    ldh [rSTAT], a      ; Load the prepared flag into rSTAT to enable the LY=LYC interrupt source 
    ld a, 96            ; Set which line to trigger the LY=LYC interrupt on by setting the rLYC register
    ldh [rLYC], a       ;  ...
    ld a, IE_VBLANK | IE_STAT ; Load the flag to enable the VBlank and STAT interrupts into A
    ldh [rIE], a        ; Load the prepared flag into the interrupt enable register
    xor a               ; Set A to zero
    ldh [rIF], a        ; Clear any lingering flags from the interrupt flag register to avoid false interrupts
    ei                  ; enable interrupts!

    ; Combine flag constants defined in hardware.inc into a single value with logical ORs and load it into A
    ; Note that some of these constants (LCDC_BG_OFF, LCDC_OBJ8, LCDC_WIN_OFF) are zero, but are included for clarity
    ld a, LCDC_ON | LCDC_BLOCK01 | LCDC_BG_ON | LCDC_OBJ_OFF | LCDC_WIN_OFF
    ldh [rLCDC], a      ; Enable and configure the LCD to show the background

LoopForever:
    call WaitVBlank     ; Call our routine which halts until the hVBlankDone flag is set
    jr LoopForever      ; Loop forever

SECTION "WaitVBlank", ROM0
; Since we have multiple interrupts enabled, using a single HALT in our mainloop isn't enough to sync the
;  mainloop to the screen refresh. Since we only have two interrupts (LY=LYC and VBlank), we could HALT twice,
;  but that scales poorly. Instead, we zero a flag, and then halt in a loop until our VBlank handler sets
;  that flag, letting us know it's fired.
WaitVBlank:
    xor a               ; Zero the hVBlankDone flag before starting our loop
    ldh [hVBlankDone], a;  ...
.loop
    halt                ; Halt the CPU, waiting until an interrupt fires (it could be STAT or VBlank)
    ldh a, [hVBlankDone]; Load the hVBlankDone flag value into A
    or a                ; This is a shortcut version of 'cp 0', to see if the flag has been set by our VBlank handler
    jr z, .loop         ; If the flag isn't set, halt again
    ret                 ; Return back to where the routine was called from

SECTION "Tile Data", ROMX
TileData:
    incbin "starry-night-tiles.2bpp" ; Include binary tile data inline using incbin
