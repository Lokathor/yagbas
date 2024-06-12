# Yagbas Language Reference

Yagbas is a programming language that targets the Game Boy and Game Boy Color.

Because of the limited capabilities of these devices, yagbas is not as flexible as a "general purpose" sort of language. It's probably easiest to explain yagbas as being "nearly assembly, but with slightly better control flow syntax". The language is intended to look kinda like Rust does, when possible.

* Source code files for yagbas must be written using UTF-8 encoded text files. Yagbas itself only relies on the Ascii subset of UTF-8, but code comments can be written using the full unicode range.
* `\r\n`, `\r`, and `\n` are all treated as "a newline" in yagbas.
* Newlines act as an implicit separation between statements in code, but whitespace is not otherwise significant. Explicit statement separation can be done with semicolon characters when desired.
* By convention semicolons are not used, and each statement is simply put on a separate source line.
* Source code files for yagbas conventionally use the `.yag` extension.

## Source Code Comments

A single line comment starts with `//` and goes to the end of the line.

A multi-line comment starts with `/*` and ends with `*/`. Multi-line comments can be nested.

A single line comment marker will "comment out" a multi-line comment close marker that appears later on the same line.

## Items

Language constructs that appear at the top level of a file are called "items". Conceptually, items are unordered within a program. One item may refer to another that is later within the source code.

### Identifiers

All items are named by a particular "identifier".

* It must start with an underscore or an ascii letter, and can be continued with underscores, ascii letters, or digits.
* There's no length limit on identifiers.
* All item identifiers must be globally unique.
* Any keyword of yagbas (such as register names, `loop`, `return`, etc) cannot be used as an identifier.
* The convention is that function names use `snake_case` naming, but this is not mandatory.

### Functions

A function declaration looks like

```
fn NAME() {
  // statements here
}
```

* The `fn` keyword marks the start of a function.
* Next is an identifier that names the function, followed by an empty set of parentheses. Future versions of the language may put things into the parentheses, but in the current version they are always empty.
* Finally, we have a set of braces around the code of the function.

Function names are conventionally in `snake_case`. The opening brace is generally placed on the same line as the function name. Indentation is generally 2 spaces per code block level.

## Statements

A function contains 1 or more statements. Statements in yagbas map fairly directly to Game Boy assembly, though some statements expand to more than one assembly instruction.

### Calling A Function

Function calls are written with the name of the function and then an empty set of parentheses.

```
function_name()
```

Similar to the declaration of a function, future versions of yagbas may place something within the parentheses, but currently they are always empty.

### Returning From A Function

To return control to the calling function use the `return` keyword.

```
return
```

### Looping

To loop many times over a block of code, use the `loop` keyword followed by the statements of the loop in braces.

```
loop {
  // statements here.
}
```
