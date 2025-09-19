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
* **Line Comments:** Line comments start with `//` and go to the end of the line.

## Token Trees

Tokens are grouped into "trees" with pairs of opening and closing markers.

* **Parens:** `(` and `)`
* **Braces:** `{` and `}`
* **Brackets:** `[` and `]`
* **Block Comments:** `/*` and `*/`

The opening and closing markers for each token tree group must match, or the
source file cannot be processed.

