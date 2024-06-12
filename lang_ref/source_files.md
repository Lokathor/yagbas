# Source Files

Source code files for yagbas must be written using UTF-8 encoded text files.
Yagbas itself only relies on the Ascii subset of UTF-8, but code comments can be
written using the full unicode range.

`\r\n`, `\r`, and `\n` are all treated as "a newline" in yagbas.

Newlines act as an implicit separation between statements in code, but
whitespace is not otherwise significant. Explicit statement separation can be
done with semicolon characters when desired.

By convention semicolons are not used, and each statement is simply put on a
separate source line.

Source code files for yagbas conventionally use the `.yag` extension.

## Comments

A single line comment starts with `//` and goes to the end of the line.

A multi-line comment starts with `/*` and ends with `*/`. Multi-line comments can be nested.

A single line comment marker will "comment out" a multi-line comment close marker that appears later on the same line.
