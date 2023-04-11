The AST-to-BIR system converts ASTs to the block-based intermediate representation (BIR).

# Use

`(compile-toplevel client some-ast)` is how this system should usually be used. Note that the function only accepts a function AST, as all IR must be within some function.

The `compile-into-module` function can be used to add a new function into an existing module after that module's initial production. This can be useful for late transformations.

# Customization

Custom ASTs will need specializations of the  `compile-ast` generic function. This function takes the client, an AST and an "inserter" as arguments. The inserter is an object that handles creation of IR. The `begin` function sets the iblock the inserter will insert into, while the `insert`, and `terminate` functions actually add instructions.

`compile-ast` should return either `nil`, indicating the AST outputs no values; a list of one BIR datum, indicating that is returned; or `:no-return`, indicating that control aborts somehow.

To smooth over handling sub-ASTs correctly, you can use `with-compiled-ast`, `with-compiled-asts`, and `with-compiled-arguments`. These macros mostly ensure that if a sub-AST of an AST always aborts, the AST itself does.
