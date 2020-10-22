# Cleavir

An implementation-independent framework for creating Common Lisp compilers. Cleavir is modular and extensible, so that CL implementations with different aims and backends can share the same code.

## Basic structure

The CST-to-AST system parses Common Lisp source code, in the form of [CSTs](https://github.com/s-expressionists/Concrete-Syntax-Tree), into objects called Abstract Syntax Trees (ASTs). ASTs closely represent the original source as `standard-object`s. They are suitable for further compilation processing, but also for other purposes like code walking and some analysis.

The AST-to-HIR system generates a High-level Intermediate Representation (HIR) from ASTs. The HIR makes program control and data flow explicit while remaining independent of the target machine. This HIR is then gradually reduced to MIR (Medium-level Intermediate Representation), which makes address calculuations explicit, and finally to machine code.

Clients may customize surface syntax by defining subclasses and methods at the AST level, and lower level operations by doing the same at the IR level. Individual modules can be used independently of one another, e.g. a code-walking application need not use anything below the AST level.
