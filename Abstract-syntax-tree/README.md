We define the abstract syntax trees (ASTs) that represent Common Lisp code. The AST is a very close representation of the source code, except that the environment is no longer present, so that there are no longer any different namespaces for functions and variables. And of course, operations such as MACROLET are not present because they only alter the environment.

The AST form is the preferred representation for some operations; in particular for the first stage of PROCEDURE INTEGRATION (sometimes called INLINING). It is the first stage of IR used by Cleavir.

# Particular AST classes

There is mostly a different type of AST for each Common Lisp
special operator, but there are some exceptions.  Here are the
Common Lisp special operators: `BLOCK`, `CATCH`, `EVAL-WHEN`, `FLET`,
`FUNCTION`, `GO`, `IF`, `LABELS`, `LET`, `LET*`, `LOAD-TIME-VALUE`, `LOCALLY`,
`MACROLET`, `MULTIPLE-VALUE-CALL`, `MULTIPLE-VALUE-PROG1`, `PROGN`, `PROGV`,
`QUOTE`, `RETURN-FROM`, `SETQ`, `SYMBOL-MACROLET`, `TAGBODY`, `THE`, `THROW`,
`UNWIND-PROTECT`.

Some of these only influence the environment and do not need a
representation as ASTs.  These are: `LOCALLY`, `MACROLET`, and
`SYMBOL-MACROLET`.

`FLET` and `LABELS` are like `LET` except that the symbols the bind are
in the function namespace, but the distinciton between namespeces
no longer exists in the AST.

A `LAMBDA` expression, either inside `(FUNCTION (LAMBDA ...))` or when
it is the `CAR` of a compound form, compiles into a `FUNCTION-AST`.
The other kind of `FUNCTION` special form, a function lookup, will end
up as either a `LEXICAL-AST` for local functions, or a `CONSTANT-FDEFINITION-AST`
for global.

We also define ASTs that do not correspond to any Common Lisp
special operators, because we simplify later code generation that
way.
