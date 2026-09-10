# Language Reference

Yagbas is a high level programming language intended for writing programs that run on the Game Boy hardware family. Other "very old hardware" targets might also be available some day.

The language is intended to be familiar to Rust progrmmers, while also keeping a reasonable scope for a one-crab hobby project.

## Builtin Types

* `()`
* `bool`
* `u8`
* `i8`
* `u16`
* `i16`
* `fx8_8` (fixed point 8.8)
* `[T; count]` (arrays)
* `*const T`
* `*mut T`
* `*vol T`

## Keywords

* `as`
* `bitbag`
* `break`
* `const`
* `continue`
* `else`
* `enum`
* `false`
* `fn`
* `for`
* `if`
* `impl`
* `in`
* `let`
* `loop`
* `match`
* `mmio`
* `ram`
* `return`
* `rom`
* `struct`
* `static`
* `true`
* `use`
* `while`
* `vol`

## Modules

In yagbas, every source file defines a "module".

Source files must contain utf-8 compatible data.

Source files are expected to use the `.yag` extension, but this doesn't affect the operation of the compiler.

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

A structure definition defines a data layout.

### Bitbags

```rust
bitbag IrqFlags {
  vblank: 0,
  lcd: 1,
  timer: 2,
  serial: 3,
  joypad: 4,
}
```

A bitbag defines names for the bits within a byte.

The individual fields are bit positions, not whole bytes, so you cannot create a pointer to a field of a bitbag.

### Enum

The language should support enums at some point, but I'm not sure of the details we want.

### Use

```rust
use core::memcpy;
```

A `use` statement brings an item from another module into scope.

### Impl

```rust
impl OamData {
  fn hide(&mut self) {
    self.y = 0;
  }
}
```

An `impl` block defines methods on a data type.

## Statements

Within a block of code (enclosed in `{ }`) there's one or more statements, and possibly a tail expression.

A statement can be one of:

* A `let` to introduce a new variable name.
* An expression.
* An item definition.

Expressions can always be followed by a semicolon to end the statement, but expressions that start with a keyword and end with a braced body are implicitly "done" when the body is over, and so do not require a semicolon after them when used as a statement. Particularly, this means that `if`, `loop`, `while`, and `for` expressions do not require a semicolon, while struct literal expressions do require a semicolon (because struct literal exlressions end with a braced body but start with an identifier instead of a keyword).

As with items in a module, item statements farther down in a block can be referred to by earlier statements.

## Expressions

The [Expression Precedence](https://doc.rust-lang.org/reference/expressions.html#expression-precedence) ordering from Rust is also used in Yagbas.
