# Notes

## Major TODOs

* `min_const_eval`: integer-only ops, for array lengths.
* Type Checking.
* Constant Evaluation.
* Split function trees into blocks.
* Split value expression trees into temp var steps.
* Allocate registers.
* Generate Assembly.
* Build a ROM image.

## Minor TODOs

* Name resolution in Path and Access ops.
* Parsing the tail expression of a block properly.
* Test coverage for the Cst layer.
* Test coverage for the Ast layer.
* Make the Cst and Ast layers more resilient when bad inputs are given.
* Track and report errors in a disciplined way.
* Support Query based compilation.

## Long Term Goals

* make steps multi-threaded by default when possible.
* additional targets: nes, snes, arm, wasm
