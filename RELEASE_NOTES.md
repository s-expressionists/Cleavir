# v2.1.0 (pending)

## Changed
* BIR now represents function and variable "cells" explicitly for linking.

# v2.0.0 (2022-11-24)

## Added
* New block-based intermediate representation (BIR).
* A verifier system to validate BIR objects.
* Test-based disassembler for BIR.
* McCLIM visualizer for BIR (contributed by @scymtym).
* AST-to-BIR system for converting ASTs to BIR.
* Attributes system for interesting non-type information about data.
* Ctype system for interface with client type system.
* Conditions system for better organization of compiler conditions.
* Example system for demos.
* Extensible metaevaluator for performing most optimizations on BIR (thanks @karlosz for this and more).

## Changed
* Some `cleavir-env` operations now take a client parameter for specialization.
* Many (most?) AST classes have changed. See documentation for details.
* CST-to-AST no longer accepts most primops, along with other more minor changes.
* CST-to-AST conditions are now part of the conditions system hierarchy. Additionally, error behavior when evaluating `eval-when` code, macros, or compiler macros has been enhanced to signal specific encapsulation conditions.

## Enhancements
* Inlining has been generalized into contification.
* Type inference now proceeds quickly and easily takes care of most basic, non-control-flow sensitive type check eliminations.
* Performance of most operations on/transformations of IR has improved.

## Removed
* High-level intermediate representation (HIR). Replaced by BIR.
* Graphviz-based IR visualizer. Replaced by the BIR disassembler and visualizer.
* AST-to-HIR. Replaced by AST-to-BIR.
* Many ASTs and IR instruction classes related to backend operations (e.g. `fixnum-add`). For now these are considered to be a client responsibility, but a future release will probably add some functionality back to Cleavir.
* SSA conversion system; this may be restored in a later release.
* IR-to-source back-conversion systems, which have never really been used.
* The code walker. It's never really been a core component, and is probably better as a separate system. See, e.g., [agnostic-lizard](https://gitlab.common-lisp.net/mraskin/agnostic-lizard) for an alternative.

# Pre-release history

Cleavir was used for several years without proper releases. The transition to 2.0 began when BIR was introduced around September 2020. The first designated version has been named 2.0 rather than 1.0 due to the severity of the changes relating to the intermediate representation.

The initial version of Cleavir was entirely due to @robert-strandh, and was an internal component of [the SICL project](https://github.com/robert-strandh/SICL). Cleavir development started in 2014 and was carried out almost solely by Dr. Strandh before 2017. Cleavir has also been used by [Clasp](https://github.com/clasp-developers-clasp) since around March 2015. Cleavir was split out of the SICL repository on 2020-09-05.

"Cleavir 1.0" uses a different intermediate representation called HIR for most operations, in addition to ASTs. It is still used and available [within SICL](https://github.com/robert-strandh/SICL/tree/master/Code/Cleavir).

## Added
* High-level intermediate representation (HIR).
* Abstract syntax tree (AST) intermediate representation.
* CST-to-AST system for converting [concrete syntax trees](https://github.com/s-expressionists/Concrete-Syntax-Tree) to ASTs.
* Systems for computing SSA conversion, liveness, reaching definitions, register allocations, dominance, type-inference, natural loops, basic blocks, and def-use chains.
* HIR transformations for closure conversions, alias elimination, etc.
* Code walker with macroexpand-all.
* More.

## Removed
* Generate-AST system for generating ASTs from source forms (rather than CSTs).
