# Language Reference

## Modules

A yagbas source file is called a "module".

## Tokens

Source code has to be tokenized first during compilation. This breaks it up into
punctuation, keywords, identifiers, and so on.

* **Identifiers:** These start with an ASCII letter or an underscore, and can
  then contain any number of ASCII letters, underscores, and digits. Identifiers
  can't be any language keyword.
* **Numbers:** Numbers can be in decimal, binary (with a leading `%`), or
  hexadecimal (with a leading `$`). Numbers can contain underscore characters
  for easier reading, this doesn't affect the value.
* **Comments:** Line comments start with `//` and go to the end of the line.
  Block comments open with `/*` and close with `*/`. Line comments will comment
  out block comment markers the same as they comment out everything else.

## Token Trees

Tokens are grouped into "trees" with pairs of opening and closing markers.

* **Parens:** `(` and `)`
* **Braces:** `{` and `}`
* **Brackets:** `[` and `]`

The opening and closing markers for each token tree group must match, or the
source file cannot be processed.

## Items

Definitions that can appear within a module are called "items".

* **Bitstruct:** Assigns field names to the bits of an 8-bit value.
* **Const:** Assigns a name to a given constant expression.
* **Function:** Defines a body of program code.
* **Static:** Defines a series of bytes that appear in the ROM.
* **Struct:** Assigns a layout for various data fields.

