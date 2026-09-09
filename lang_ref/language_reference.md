# Language Reference

Yagbas is a high level programming language intended for writing programs that run on the Game Boy hardware family. Other "very old hardware" targets might also be available some day.

The language is intended to be familiar to Rust progrmmers, while also keeping a reasonable scope for a one-crab hobby project.

## Modules

In yagbas, every source file is a "module".

## Items

An "item" is a program element that can exist on its own within a module.

All items in a module are considered to be defined "simultaneously". This means that an item can refer to another that's defined later on in the source file.

## Item Kinds

### Constants

```rust
const NAME: Type = expression;
```

Defines a name for a compile-time value.

### Statics

```rust
static mmio($FF40) LCDC: LcdControl;

static ram SCORE: u8 = 0;

static rom BASE_HP: [u8; 5] = [0, 3, 5, 12, 10];
```

Statics define data available at runtime.

* mmio loctions are memory-mapped IO access.
* ram locations are mutable runtime data.
* rom locations are read-only runtime data.

### Functions

```rust
fn add_two(x: u8) -> u8 {
  x + 2
}
```

Functions create executable code.

Every program must define a `main` function, which is the start of exectution.

### Structs

```rust
struct OamData {
  y: u8,
  x: u8,
  tile_index: u8,
  attributes: OamAttrs,
}
```

