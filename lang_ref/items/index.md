# Items

Language constructs that appear at the top level of a file are called "items".

Conceptually, items are unordered within a program. One item may refer to
another that is later within the source code.

## Identifiers

All items are named by a particular "identifier".

An identifier must start with an underscore or an ascii letter, and can be continued with underscores, ascii letters, or digits.

There's no length limit on identifiers.

All item identifiers must be globally unique.

Any keyword of yagbas (such as register names, `loop`, `return`, etc) cannot be used as an identifier.
