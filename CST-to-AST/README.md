The CST-to-AST system converts CSTs to one of Cleavir's intermediate representation's, "abstract syntax trees" (ASTs). An AST is a representation of the code as an easily manipulable tree of standard objects roughly corresponding to the original code's tree organization.

# Basic use

The main entry point is the `cst-to-ast` function. This function takes three parameters: the CST, and the environmentand system to compile it with respect to. It converts the CST to an AST and returns it.

The `cst-to-ast:*compiler*` variable should be bound to indicate to CST-to-AST how it should deal with top level forms and `load-time-value`. The value can be any of the symbols `cl:eval`, `cl:compile`, or `cl:compile-file`.

A wide variety of conditions can be signaled if the input code is not syntactically correct; see "Conditions" below.

For an example of usage, check the cleavir-example system.

# Environment interface

In order to convert CSTs, CST-to-AST must sometimes get information from the environment. It does this by using the Environment system. Briefly, here are the generic functions that must be specialized:

* `variable-info`: Returns information about variables.
* `function-info`: Retrieves information about operators (functions, macros, special operators).
* `optimize-info`: Retrives information about current optimization settings, as well as current policy (see the cleavir-compilation-policy system).
* `declaration`: Retrieves the list of valid nonstandard declarations.
* `type-expand`: Analogously to `cl:macroexpand`, expands macro (`deftype`-defined) type specifiers. This is required for CST-to-AST to do any type specifier parsing, which it may need to do even if the program contains no type declarations. Type specifier parsing can be further customized via the other generic functions in cleavir-environment, as well as the cleavir-ctype system, but this is not necessary.
* `eval`: Evaluates a form. This is used in conversion of `cl:macrolet` to produce the local macroexpander.
* `cst-eval`: Evaluates a CST of a form. This is used to execute compile-time side effects.

# Conditions

CST-to-AST can signal any of dozens of different conditions when it is passed syntactically incorrect source code. The complete listing is in conditions.lisp. Acclimation reporters are defined for all conditions in English, but no other languages at the moment.

All conditions are subtypes of the general `cleavir-conditions:program-condition`. Besides that, each condition type inherits from one of three abstract condition types particular to CST-to-AST: `compilation-program-error`, `compilation-warning`, and `compilation-style-warning`. These are errors, warnings, and style-warnings respectively, and they are used as described in CLHS 3.2.5 "Exceptional Situations in the Compiler". That is,

* `compilation-program-error` indicates that CST-to-AST cannot proceed without intervention. If unhandled, compilation fails.
* `compilation-warning` indicates that CST-to-AST has run into code that it can compile, but which has undefined effects or is otherwise proscribed by the language standard. It also represents a compilation failure.
* `compilation-style-warning` indicates that CST-to-AST has run into valid but subpar code. It does not represent a compilation failure.

Also notable is the concept of _encapsulated_ conditions. This is an orthogonal hierarchy to the error/warning/style-warning distinction, headed by the `encapsulated-condition` condition type. When CST-to-AST runs a macro or compiler macro function, and that function signals an error, warning, or style warning, CST-to-AST will intercept that condition and resignal its own encapsulated condition. This is so that clients can respond to such errors uniformly. For example, if a compiler macro function signals an error, CST-to-AST will signal a `compiler-macro-expansion-error`; the client can then, if it chooses, handle this by selecting a restart (more on that below) that tells CST-to-AST to not attempt the compiler macro expansion.

CST-to-AST makes several kinds of restart available. In general, the `continue` restart can be used to proceed with some kind of defaulted action. Any condition signaled during conversion can be resolved with the `substitute-cst` restart, which tells CST-to-AST to continue with the provided CST instead of the one that resulted in the condition being signaled.

CST-to-AST signals errors in situations where most clients may not wish to propagate errors to the user. For example, if CST-to-AST sees a variable not known from the environment, it will signal an error. If a client instead wishes for something less severe, like proceeding under the assumption that the variable is special, it will have to handle the error. The most notable errors of this kind are:

* `no-variable-info`: indicates that a variable is unknown. CST-to-AST can be made to proceed under the assumption that the variable is special by use of the `continue` or `consider-special` restarts.
* `no-function-info`: indicates that an operator is unknown. CST-to-AST can be made to proceed under the assumption that the operator names a global function by use of the `consider-global` restart.
* `compiler-macro-expansion-error`: indicates that a compiler macro function signaled an error. CST-to-AST can be made to ignore the macro and proceed with the original call by use of the `continue` restart.

# Customization

To add additional special operators, the `convert-special` function should be specialized. This function takes four arguments: The name of the operator (a symbol), the CST of the special form, the environment and the system. Methods should parse the special form CST by whatever means peculiar to the operator's syntax, and return some AST representing the form. Subforms should be converted into CSTs using `convert`. Note that merely defining a `convert-special` method is not sufficient for CST-to-AST to understand a symbol to be a special operator: you will also need `cleavir-env:function-info` to report that it is a special operator.

# Handling of types

The `type-wrap`, `type-wrap-argument`, and `type-wrap-return-values` generic functions determine how CST-to-AST handles type declarations via `cl:the` and declarations. By default, nothing is done, i.e. CST-to-AST accepts but ignores them. A client may specialize these methods in order to store type declarations in the AST.

CST-to-AST handles declarations very literally according to the language specification: a type declaration on a variable is treated just as if all uses of that variable had an appropriate `cl:the`.
