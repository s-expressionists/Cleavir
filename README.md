# Cleavir

Cleavir is an implementation-independent framework for creating Common Lisp compilers. It is modular and extensible, so that CL implementations with different aims and backends can share the same code.

## Getting started

The `Example/` directory contains a worked example of a simplified Cleavir client, showing how Cleavir can be used to translate Common Lisp code into an intermediate representation, and how optimizations can be applied to this IR. The README there explains how to load and use that system.

## Basic structure

Cleavir is not a single system. It is instead a collection of systems that can be incorporated together as subsystems of a compiler.

The CST-to-AST system parses Common Lisp source code, in the form of [CSTs](https://github.com/s-expressionists/Concrete-Syntax-Tree), into objects called Abstract Syntax Trees (ASTs). ASTs closely represent the original source as `standard-object`s. They are suitable for further compilation processing, but also for other purposes like code walking and some analysis.

The AST-to-BIR system generates a Block-based Intermediate Representation (BIR) from ASTs. The BIR makes program control and data flow explicit while remaining independent of the target machine. More information on BIR is available in the `BIR/` directory.

The BIR-transformations system analyzes and applies various optimizations to BIR. More information on the transformations Cleavir defines are available in the `BIR-transformations/` directory.

Clients may customize surface syntax by defining subclasses and methods at the AST level, and lower level operations by doing the same at the IR level. Individual modules can be used independently of one another, e.g. a code-walking application need not use anything below the AST level.
