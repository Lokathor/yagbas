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

