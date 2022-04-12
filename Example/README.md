Cleavir is a collection of parts that can be used to make a Lisp compiler. A Lisp compiler is closely linked to the Lisp runtime, both in that it must target the runtime, and it must sometimes actually use it to evaluate Lisp code. As such, Cleavir cannot operate as an independent system.

This makes it harder to get started on understanding and using Cleavir. To rectify this problem, this directory defines a small system making a subset of Common Lisp available, and demonstrates how to use Cleavir as a compiler for this system.

# Getting started

1. Load the `cleavir-example` system. Besides Cleavir systems, it depends on the external systems `concrete-syntax-tree` and `acclimation`, both of which should be available from Quicklisp.
2. Run `(cleavir-example:load-environment)` to set up the compilation environment.
3. You can now compile a subset of Lisp code to BIR (Cleavir's intermediate representation). Try `(cleavir-example:frontend (cst:cst-from-expression '(lambda (x) x)))`, for example.
4. This will return a BIR "module". You can display a disassembly of this module with `(cleavir-bir-disassembler:display module)`.

# Using the visualizer

This example system can be used with the BIR visualizer (in BIR/Visualizer) as follows:

1. Load the `cleavir-example` and `cleavir-bir-visualizer` systems.
2. Run `(cleavir-example:load-environment)` to ste up the compilation environment.
3. Run `(cleavir.bir.visualizer:run :environment cleavir-example:*environment* :system cleavir-example:*system*)` to start the visualizer. `:new-process t` can additionally be passed in order to run the visualizer in a new thread, freeing up your REPL.
4. Enter code in the "Lambda Expression" field (or leave the default), use the Safe/Default/Fast buttons and the optimize sliders to run a compilation, and freely inspect the IR in the "Intermediate Representation" tab.

# Limitations

The example system only includes a subset of Common Lisp. Most obviously, only a few basic macros like `lambda`, `cond`, and `dolist` are defined (the complete listing is in macros.lisp). The example system must define all of its own macros, because host system macros (i.e. the macros defined by your Lisp implementation) may expand into nonstandard code that the example system does not know how to process.

Some standard special operators are not implemented by the basic Cleavir system due to requiring some runtime coordination. These are `catch`, `progv`, `throw`, and `unwind-protect`. Similarly, special variable binding is by default implemented as a function call. The example client implements the special operators as macros expanding into function calls. A full client would probably specify dedicated ASTs and IR for representing these operators.

Cleavir does not include a backend, since a real full backend more or less requires an entire accompanying Lisp implementation, in order for the details of how code is laid out and run, etc., to be defined. Clients of Cleavir can define their own backends. For example, [Clasp](https://github.com/clasp-developers/clasp) translates BIR into LLVM-IR, which is then passed to LLVM to generate machine code.
