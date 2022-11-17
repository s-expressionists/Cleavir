`cleavir-conditions` is a small system basically defining two hierarchies of conditions: `program-condition` and `compiler-note`.

# Program conditions

Program conditions (`program-error`, `program-warning`, etc.) are conditions indicating a problem in the source code passed in for compilation or other processing. For example, forms like `(cons 1 2 3)` or `(progn . 4)` could induce program conditions.

There are two basic reasons for these conditions. One is so that users of Cleavir can distinguish problems found by Cleavir in its input from problems Cleavir itself runs into. A program condition does not indicate any kind of problem in Cleavir but rather a problem with its input, whereas some other condition might be a bug in Cleavir. Secondly, all program conditions have an associated source location indicating the part of the input program with the issue. This source location can be read with the `origin` function. Clients defining their own program conditions may wish to define methods on this function.

# Compiler notes

Notes (`program-note`) can be used by compilers for conditions not warranting even a style warning. This could include compiler-specific optimization hints, or notes that the compiler will ignore some declaration because it is not smart enough to use it, for example. Essentially, they do not indicate a problem with the code itself per se, but the compiler is not doing its best with it.
