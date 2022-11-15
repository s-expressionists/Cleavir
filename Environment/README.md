The `cleavir-env` system defines an interface Cleavir can use to query information from client environments. It also defines lexical environment objects used internally when processing code (e.g. for `cleavir-cst-to-ast`).

This system is slated to be phased out in favor of [Trucler](https://github.com/s-expressionists/Trucler/). The only unresolved sticking point is that Cleavir sometimes wants to stick its own information into lexical environments, and this is not possible for unprepared clients.

# Minimal definitions by clients

Briefly, here are the generic functions that must be specialized in order for `cleavir-cst-to-ast` to work:

* `variable-info`: Returns information about variables.
* `function-info`: Retrieves information about operators (functions, macros, special operators).
* `optimize-info`: Retrives information about current optimization settings, as well as current policy (see the cleavir-compilation-policy system).
* `declaration`: Retrieves the list of valid nonstandard declarations.
* `type-expand`: Analogously to `cl:macroexpand`, expands macro (`deftype`-defined) type specifiers. This is required for CST-to-AST to do any type specifier parsing, which it may need to do even if the program contains no type declarations. Type specifier parsing can be further customized via the other generic functions in cleavir-environment, as well as the cleavir-ctype system, but this is not necessary.
* `eval`: Evaluates a form. This is used in conversion of `cl:macrolet` to produce the local macroexpander.
* `cst-eval`: Evaluates a CST of a form. This is used to execute compile-time side effects.

An example set of method definitions can be seen in the `cleavir-example` system.
