# Language Reference

## Tokens

### Comments

Multi line comments start with `/*` and end with `*/`. They can be nested within each other.

```rs
/* multi line
comment */
```

Single line comments start with `//` and go to the end of the line.

```rs
// single line comment
```

A single line comment marker takes priority over multi line comment markers, and
will "comment out" a multi line comment open or close marker that appears after
it on the same line.

### Registers

Register names can be all lowercase or all caps.

* `a` / `A`
* `f` / `F`
* `b` / `B`
* `c` / `C`
* `d` / `D`
* `e` / `E`
* `h` / `H`
* `l` / `L`
* `af` / `AF`
* `bc` / `BC`
* `de` / `DE`
* `hl` / `HL`

### Conditions

Conditions can similarly be all lowercase or all caps

* `c` / `C`
* `z` / `Z`
* `nc` / `NC`
* `nz` / `NZ`

Note that `c` is both a register name and a condition name. The meaning of the
token is always sufficiently clear from the surrounding context.

### Keywords

All other keywords only allow a lowercase spelling

* `break`
* `const`
* `continue`
* `fn`
* `loop`
* `return`
* `static`

### Numbers

Numbers can be written in decimal, hexadecimal, or binary.

* Decimal numbers must start with a digit.
* Hexadecimal numbers are prefixed with `$`
* Binary numbers are prefixed with `%`

```
123
$AB12
%101011
```

Underscores can be used within a number literal, and they don't affect the value
the literal represents.

```
1_234 == 1234
```

### Identifiers

Identifiers in Yagbas work essentially like Rust and C style identifiers.

* They must start with an underscore or an ascii letter.
* They can contain underscores, ascii letters, or digits.
* They can't be any other keyword, such as `a`, `nz`, or `break`.

### Token Tree Groupings

Certain punctuation will open and close a "token tree" group.

* `(` and `)`
* `{` and `}`
* `[` and `]`
* `/*` and `*/`

Token trees can contain inner token trees, but the group markers must balance
out. If they are unbalanced then essentially the entire rest of the file cannot
be processed by the parser.

## Items

Items are defined at the top level of a source file.

The order that items are defined in has no effect.

### Functions

Functions define executable code that runs as part of the program.

```
fn NAME() {
  // statement 1
  // statement 2
  // ...
  // statement n
}
```

Each function has a name and a body. The body of the function contains the
statements to execute in order to make the program do whatever it's supposed to.

Statements are written one per line. If more than one statement is desired on
the same line then a `;` can be used, but this is not the standard style.

## Expressions

Expressions are used primarily to evaluate constant values for use during compilation.

Constant expressions are number literal values, constant identifiers, or
parenthesis groups holding a constant expression. Constant expressions can be
combined together using various math operators.

Runtime expressions allow register names, and in some cases allow operators to
be used to combine constants with a register value.

Operators in Yagbas follow the same precedence ordering used by Rust. Operators
at the same precedence level work left to right by default.

* Unary `-` (negation)
* `+` and `-`

## Statements

### Calls and Returns

Function calls are written with the name of the function call followed by parenthesis.

```
some_function()
```

A function returns to its caller with the `return` keyword.

```
return
```

### Loop, Break, and Continue

A loop is used to execute a block of statements over and over.

```
loop {
  // statement 1
  // statement 2
  // ...
  // statement n
}
```

Within a loop, the `break` keyword can be used to skip the rest of the loop body
and jump to after the loop. Similarly, the `continue` keyword can be used to
jump back to the start of the loop body. When one loop is contained in another,
`break` and `continue` go to the end or start of the innermost loop.

Loops can optionally have a name given to them:

```
'name: loop {
  // ...
}
```

When a loop has a name, then `break` and `continue` can go to the end or start
of that loop by giving that name after the keyword.

```
'foo: loop {
  loop {
    break 'foo // this goes to the end of the "foo" loop
  }
  return // this line is skipped by the `break` above.
}
```

This allows the program to jump from one loop nested inside of another "all the
way" to the start or end of an outer loop.

The `break` and `continue` keywords cannot be used outside of a loop.
