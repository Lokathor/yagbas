# Linking

Once a section is evaluated it will have the following info:

* A list of bytes (some of which may be "dummy" bytes, see below).
* A set of labels it contains (their name and offset within the section)
* A list of expressions that require labels from other sections to evaluate, and which dummy bytes they replace once evaluated.

Then the following steps must be performed:

1) All of these sections must be placed into the ROM and/or RAM address space. This gives a specific value to all labels.
2) All the required expressions of all placed sections are evaluated using the new location information.
3) All dummy bytes placed previously are updated with their final value.
