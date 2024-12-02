# Language Reference

## Tokens

Yagbas source code is read as utf8 data, and is then lexed into "tokens", which
make up the program.

### Comments

Single line comments start with `//` and go to the end of the line.

```rs
// single line comment
```

Multi line comments start with `/*` and end with `*/`. They can be nested within each other.

```rs
/* multi line
comment */
```

A single line comment marker takes priority over multi line comment markers, and
will "comment out" a multi line comment open or close marker that appears after
it on the same line.

Commented text has no effect on the program.

### Registers and Conditions

Register names and Condition values can be written in uppercase or lowercase.

* `a` / `A`
* `af` / `AF`
* `b` / `B`
* `bc` / `BC`
* `c` / `C` (used for both the register and the "carry" condition)
* `d` / `D`
* `de` / `DE`
* `e` / `E`
* `f` / `F`
* `h` / `H`
* `hl` / `HL`
* `l` / `L`
* `nc` / `NC` (no-carry condition)
* `nz` / `NZ` (non-zero condition)
* `z` / `Z` (zero condition)

### Other Keywords

All other keywords must be written in lowercase.

* `break`
* `const`
* `continue`
* `else`
* `false`
* `fn`
* `if`
* `loop`
* `mut`
* `return`
* `static`
* `true`

### Number Literals

Numbers can be written in decimal, hexadecimal, or binary.

* Decimal numbers must start with a decimal digit.
* Hexadecimal numbers are prefixed with `$`, and then 1 or more hexadecimal
  digits. Both uppercase and lowercase are allowed for digits `a` through `f`.
* Binary numbers are prefixed with `%`, then either binary digit.

```
123
$AB12
%101011
```

Underscores can also be used within a number literal. They don't affect the
value the literal represents, but they can make the number easier for the
programmer to read.

```
1_024 == 1024
```

### Identifiers

Identifiers in Yagbas work the same as in languages such as Rust, C, Java, etc.

* They must start with an underscore or an ascii letter.
* They can contain underscores, ascii letters, or decimal digits.

## Token Trees

After tokenization, the tokens are arranged into "token trees". A token tree is
either a "lone" token, or a list of token trees contained within one of the
types of grouping markers.

* `(` and `)`, "parens"
* `{` and `}`, "braces"
* `[` and `]`, "brackets"
* `/*` and `*/`, "multi-line comments"

The group markers must balance out or the token trees cannot be constructed
correctly. The yagbas parser is able to recover from some parsing problems, but
unbalanced token trees will often halt it in its tracks completely.

## Items

Items are the things that a source file defines.

Items are conceptually unordered. They can refer to other items either before or
after their own definition, and it makes no difference.

Each item has a name. No two items can have the same name.

### Consts

A const names an expression so that it can be used elsewhere within the program.

```
const NAME = EXPRESSION
```

### Statics

A static value declares bytes of non-code data that should be present for the
program to use. This is how you declare tile data or other program data.

A list of individual byte expressions appears in a comma separated list inside square brackets.

```
static NAME = [byte0, byte1, byte2, ..., byteN]
```

When necessary, the list can be broken across several lines. The final
expression can also optionally have a trailing comma.

```
static NAME = [
  byte0, byte1, byte2,
  byte3, byte4, byte5,
  byte6, ....., byteN,
]
```

### Functions

Functions define executable code that can be run during the program.

```
fn NAME() {
  statement0
  statement1
  ...
  statementN
}
```

Each function has a name and a body.

The body of the function consists of braces enclosing the statements to execute
in order to perform the function's task. Statements are separated by either a
newline or a semicolon.

## Statements

### Expression Statements

An expression statement performs some expression, generally an assignment, as a
statement.

```
a = 0
b = 5
```

### If-Else

If-else statements let some code be conditionally performed.

```
if a < 20 {
  b = 15
} else {
  b = 20
}
```

The `else` portion can be omitted.

```
if a == 0 {
  b = 10
}
```

As a small exception to the usual parsing of only one statement per newline or
semicolon, there *can* be newlines after the braces of the `if` body and before
the `else` keyword.

```
if a != 1 {
  b = 3
}
else {
  b = 4
}
```

### Loop, Break, and Continue

A `loop` statement contains a list of inner statements.

```
loop {
  // ...
}
```

* The statements of the loop are performed, and then control flow goes back to
  the start of the loop.
* `break` will skip control flow to the end of the loop.
* `continue` will move control flow back to the start of the loop.
* In both cases, when a loop contains another loop, they move to the start or
  end of the inner-most loop.

Loops can be named, allowing for `break` and `continue` to go to a loop outside
the inner-most loop.

```
'outer: loop {
  'next: loop {
    a = b
    loop {
      if a == 0 {
        break 'next
      } else {
        break 'outer
      }
    }
  }
}
```

### Call and Return

A function can be called to move control flow from the current function to that
other function. The function to call is named, followed by a pair of
parenthesis.

```
foo()
```

When a function has completed its task it can `return` to the function that called it.

```
fn set_a_to_zero() {
  a = 0
  return
}
```

## Expressions

TODO
