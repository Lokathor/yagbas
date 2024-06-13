# Functions

A function declaration looks like

```
fn NAME() {
  // statements here
}
```

The `fn` keyword marks the start of a function.

Next is an identifier that names the function, followed by an empty set of
parentheses. Future versions of the language may put things into the
parentheses, but in the current version they are always empty.

Finally, we have a set of braces around the code of the function.

The conventional code style for functions is as follows:

* Names are `snake_case`
* Indentation is 2 spaces
* The opening brace is on the same line as the function name

## `fn main()`

The `main` function has a special meaning.

After booting, the Game Boy jumps to the "Entry Point" of the ROM ($0100) and
begins running there. However, the entry point can only be 4 bytes long, because
after that is the rest of the ROM header rather than meaningful instructions.
The Entry Point is expected to jump to the actual beginning of the program.

The yagbas compiler always uses `di; jp main` as the effective code for the
entry point. If your program does not include a `main` function, compilation
will cause an error.
