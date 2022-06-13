This directory contains code to analyze and transform BIR, Cleavir's intermediate representation. (More information on BIR is available in the BIR directory.) The passes form a bit of a "grab bag" in that they are not closely linked to one another.

# eliminate come-froms

Main entry points: `module-eliminate-come-froms`, `eliminate-come-froms` (per-function version)

This pass removes unneeded catch instructions. A catch is unneeded if nothing unwinds to it. These unneeded come-froms occur because AST-to-BIR does not know in advance whether each `cl:block` or `cl:tagbody` is unwound to, so it inserts a catch for every one. Come-Froms are removed by replacing them with jumps, and if possible, merging the iblocks together.

At this time, no other transformation adds come-froms, and even if they did, it's likely they could be selectively added only if unwinding is actually possible. Running this pass is therefore recommended. However, dead code elimination could eliminate unwinds, therefore making this pass potentially useful later as well.

# process captured variables

Main entry points: `determine-function-environments`, `determine-closure-extents`, `determine-variable-extents`

This pass determines what variables are closed over by what functions, and also determines the extents of variables and closures. This includes intermediate functions, e.g. in `(lambda (x) (lambda () (lambda () x)))`, the middle function needs to close over `x` despite not reading it directly. Closed over variables are recorded in the closure functions' `bir:environment`s.

The "extent" of a variable or enclosure is an indication of what space they can be used in; this is the same as the "extent" in the Common Lisp standard. There are three possible extents here:

* `:local` only applies to variables. It indicates that the variable is only ever used in one function. That is to say, it is not closed over. Local variables do not need to have any real identity, and can be compiled e.g. as registers or stack locations.
* `:dynamic` indicates that a variable or closure has dynamic extent. Specifically, it never escapes the function it is bound or created in. In `(let ((y ...)) (mapcar (lambda (x) (cons x y)) L))`, for example, `y` has dynamic extent: while its value may escape, the variable itself does not. Dynamic extent variables or closures can be compiled to exist in stack storage.
* `:indefinite` extent variables and closures have no limits that Cleavir can discern. They probably need to translate to heap allocations only deallocated by the garbage collector.

`:local` is more desirable than `:dynamic`, which is in turn more desirable than `:indefinite`. This is because the former are more flexible, and could be implemented more efficiently. It is perfectly possible for a client to opt not to use this information, of course.

It is strongly recommended that these functions are run in the order `determine-function-environments`, `determine-closure-extents`, `determine-variable-extents` to maximize analysis, i.e. to mark as many variables and closures as possible as `:local` or `:dynamic`. It is also recommended that these be run as late as possible, in order to benefit maximally from other transformations deleting code etc. Other BIR-transformations in this directory do not utilize extent information - that's largely a matter for lower level code generation.

Closure extents rely in part on the `:dx-call` attribute of functions. In the above example with `mapcar`, Cleavir will only analyze `y` as having `:dynamic` extent if it is informed that `mapcar` has the `:dx-call` attribute. Without this information, Cleavir will not know that `mapcar` does not, for example, store its closure argument in a global variable. Clients may choose which functions to mark with this attribute, as it can depend on parts of the implementation otherwise outside a compiler's purview.

# delete temporary variables

Main entry points: `module-optimize-variables`, `function-optimize-variables`

This is a very simple pass that just deletes variables with no read instructions. A warning will be issued (indirectly, by hooks on `delete-instruction`) if a variable was not declared `ignore` or `ignorable`.

# simple unwind

Main entry points: `simple-unwinding-p`

This analysis determines if unwinds are "simple" in the sense that the dynamic environments it unwinds through can be understood statically. More detailed information is available in simple-unwind.lisp.

The generic function `simple-dynenv-p` can be used to extend the analysis. Cleavir tries to make as few assumptions about the runtime as possible, so by default it will not treat even very basic examples as simple- for example, in (block a (block b ... (return-from a)))`, if the intermediate block's `catch` instruction continues to exist, this pass will report that the `return-from` is not simple. Clients should define methods on `simple-dynenv-p` in accordance with their runtime in order to let Cleavir identify more unwinds as simple.

Unlike most other transforms, this pass does no caching on its own - it does not modify anything in the BIR graph itself. This is because most programs have few unwinds anyway so the compile-time cost of redundant analyses is not too high.

Simple unwind information can be used very late in the compilation process, in code generation. Because there is no caching, there is no point in running this analysis until it is actually needed.

# generate type checks

Main entry points: `module-generate-type-checks`

This function converts `thei` instructions equipped with an actual runtime test into that test. Specifically, any `thei` with a `type-check-function` that is an actual BIR function will be replaced with a `local-mv-call` to that function.

Because meta-evaluate can move, delete, and create type checks, it is reocmmended that type checks are only generated later in compilation.

# inline

Main entry points: `find-module-local-calls`

This pass transforms `call`s into `local-call`s where possible. It will find `enclose` instructions that lead directly into `calls`, and delete the `enclose` and mutate the `call` into a `local-call`.

As described in the BIR README, marking as many calls as possible local is very important in order for other analyses to proceed effectively. In particular, local calls should be marked before meta-evaluate runs.

After marking local calls for a function, the inline pass will automatically try to interpolate it (below).

The inliner tries to mark only valid calls, i.e. calls that don't have too many or too few arguments. It doesn't check keywords, and if it encounters a nonstandard lambda list keyword, it gives up and does not perform the marking.

# interpolate function

Main entry points: `maybe-interpolate`, but usually called automatically by the inlining pass (above)

This pass interpolates functions into other functions. This means that the function being interpolated is deleted from the module, and its body replaces the call to it. As such, this may be considered the true "inlining" pass.

The interpolator can only handle some kinds of functions. If a function lambda list contains `&key` or an unknown lambda list keyword, interpolation will not occur. `&rest` will also prevent interpolation, unless the rest parameter is not actually used by the function to be interpolated.

Basically, only functions with exactly one local call and no enclose will be interpolated, because interpolation destroys the function IR. That said, the interpolator also runs a simple contification analysis - more specifically, the A_call analysis described in Fluet and Weeks 2001. Contification allows the interpolator to work even if there are multiple calls, and on tail recursive functions, provided the calls have the same continuation, or alternately if the function never returns normally (rendering the return continuation abandoned). Among other things, contification allows Cleavir to inline tail recursive functions into loops, facilitating functional programming. Consult the paper for more information.

Differing calls can only be contified if they have the same dynamic environment. For example, in `(flet ((foo ...)) (block nil (if x (foo) (multiple-value-prog1 (bar) (when y (return (foo)))))))`, Cleavir may not be able to contify `foo` because the `multiple-value-prog1` establishes a dynamic environment around the second call, even though both calls end up returning from the same block.

# meta evaluate

Main entry points: `meta-evaluate-module`

The meta evaluate pass is where a wide variety of transformations and analyses can occur. Meta evaluation propagates information through data (but generally not control) flow, and applies optimizations that could apply to expressions. For example, meta evaluation carries out constant folding, and transformations of calls peculiar to the function being called.

At present, meta evaluation proceeds very simply by passing through functions in forward data flow order three times. This may change to a more sophisticated data flow in the future. Dead code elimination by other parts of meta evaluation may allow pseudo-control-flow dependent propagation in specific circumstances.

Parts of meta evaluate include:

## type and attribute propagation

Meta evaluation propagates type and attributes of data forward. Three kinds of data are currently propagated:

* _derived_ types, meaning type information that the compiler has proven is correct, and so may be used to drive otherwise-unsafe transformations
* _asserted_ types, meaning type information provided in the program through declarations that may or may not be correct, but which may drive transformations
* _attributes_, meaning non-type information useful to the compiler, including flags like `:flushable`, constant folding information, information about particular transformations, and type derivers, all variously described below.

At present, propagation does not take control flow into account at all. This means for example that given `(loop while (typep x 'foo) ...)`, Cleavir is not able to determine that `x` is a `foo` in the loop unless there is information more globally restricting its type.

### derived versus asserted types, and type check motion

Cleavir tracks declared (asserted by the programmer) types separately from types it has actually proven must hold. Derived types may be used to perform transformations that would not be correct if the types were inaccurate, such as folding conditionals. Asserted types may or may not hold, but specify programmer intent regardless. For example, given `(lambda (x) (declare (single-float x)) (+ x x))`, a client in coordination with Cleavir could safely transform this into `(lambda (x) (unless (typep x 'single-float) ...type error...) (unsafe-single-float-+ x x))` if desired.

A mechanism is also provided for the programmer or client to make "trusted" type declarations that meta evaluation will treat as derived/proven. The `cleavir-primop:truly-the` special operator does this, as well as any `thei` instruction with a `:trusted` type check function. This mechanism must be used carefully, as unsafe transformations it allows can cause a wide variety of bizarre effects if the type declaration does not actually hold.

Meta evaluation may promote asserted types to derived types, either through the client-dependent transformations described below, or with respect to `typeq`. If meta evaluation encounters a `typeq` instruction that would always go one way or the other based on the tested type, it will call a generic function, `generate-type-check-function`. If this function returns `nil` (the default), meta evaluation will not transform the `typeq`. If the client has defined a method on this function that returns a BIR function that performs a type check, however, meta evaluation will replace the `typeq` with a checked `thei` instruction. This allows the declared-impossible branch to be deleted without compromising safety.

Type checks may be moved, or "lifted", by meta evaluation, so that they occur earlier in the control flow. This is only done if the _derived_ type is a subtype of the _asserted_ type, so that type checks do not move outside of where they have been declared. As an example, in `(let ((x (foo))) (write x) (let ((y x)) (declare (single-float y)) (print y) (if (typep y 'single-float) ...)))`, if the `typep` is converted into a type check, that type check may be moved before the `print` call, but not before the `write` call. Besides catching problems early, this mechanism also allows type checks to be moved out of loops.

### attributes

Attributes are how Cleavir packages information useful to the compiler not expressible in the type system. They are passed along and derived similarly to types, so that e.g. in `(let ((f #'+)) ...)` Cleavir is not tripped up and understands `f` to be the `+` function. At present, all attribute information only concerns functions, so other values do not have useful attributes.

Attributes are described in more detail in the Attributes/ system, but the primary user of attributes within Cleavir is meta evaluation. There are two parts to an attributes object: flags, and identities.

As mentioned below in "flushing", the `:flushable` attribute communicates that Cleavir can validly delete calls to the given function. This is the only use of attributes in meta evaluation presently.

The "identities" drive much more information. The identities slot is designed to communicate information particular to the function through some kind of identifier for that function. At present, the identity is hooked into three parts of meta evaluation: constant folding, type derivation, and transformations, all described below.

### client type derivation

There is a hook present allowing clients to specify details of the types of certain function calls that cannot be expressed in the standard type system. For example, a client could indicate to Cleavir that `(float (the integer x))` returns a single float.

The hooking works as follows. First, the function in question must have an identity attached to it; this can be done in the BIR production stages (especially environment queries). Then, the client must specialize the `derive-return-type` generic function. Meta evaluation will pass this function four arguments: a call instruction, the identity from the attributes, the values ctype describing the arguments to the function, and a specializable system parameter. This function is expected to return the ctype derived for that function.

Note that the returned type is in general (always, at the moment) treated as proven by meta evaluation, i.e. will not be checked. As such this mechanism should only be used for standard functions or functions that cannot be redefined such that the type information is incorrect, or else the compiled code may behave badly.

It is recommended that the return type be computed from the arguments type rather than by looking at the BIR directly. `derive-return-type` may be called for both `call` and `mv-call` instructions, and in the latter case more information about the arguments may not be available. Types being derived even for variadic calls is intended to facilitate `multiple-value-call`, and possibly derived operators such as `apply`, working smoothly with type inference.

## flushing

Meta evaluation will delete instructions with unused values when those instructions are validly flushable (generally, that they do not have side effects). The `:flushable` attribute of functions may be used to indicate to Cleavir that calls to that function may be validly flushed. (A word of warning - many functions may not always be flushable, because they are expected to signal type or other errors even if their result is unused.)

## if folding

Meta evaluation will remove several kinds of `ifi` conditionals. Most prominently, conditional tests may be folded into constants if the compiler can prove they are always true or always first; this is described for `typeq` in particular above. If meta evaluation then finds a conditional with a known boolean value as input, it will replace the `ifi` with an unconditional jump, and if possible merge the iblocks and delete the never-reached branch.

"if-if" code is also deleted, i.e. `(if (if x y z) a b)` is transformed into `(if x (if y a b) (if z a b))` (but without duplicating code). if-ifs can be readily produced from certain constructs, especially with inlining or other transforms of standard functions; for example if `not` is defined as `(defun not (x) (if x nil t))`, `(if (not a) ...)` becomes `(if (if a nil t) ...)`.. Eliminating if-ifs is important to allowing other analyses and transformations to proceed.

Finally, meta evaluation also eliminates "degenerate" conditionals. A conditional is degenerate if both branches are the same iblock. This can again arise from various transformations, and removing it substantially helps other meta evaluations (not to mention removes a branch at runtime). Degenerate conditionals are replaced with unconditional jumps.

## variable substitution

Variables with only one definition (binding) and read are removed, as in transforming `(let ((x (foo))) (bar x))` into `(bar (foo))`.

## constant propagation

Meta evaluation will propagate constants through basic instructions such as single definition/use variables or `fixed-to-multiple`.

A mechanism is also provided for clients to tell Cleavir to constant-fold certain function calls. First, the function must be equipped with an identity attribute, as described above. Then, the client must specialize the `fold-call` generic function. When it finds a function call with constant arguments, meta evaluation will call `fold-call` with four arguments: the system, the identity, the call instruction, and the list of arguments (i.e. the actual values). `fold-call` may return a primary value of false, in which case no folding is performed, or it may return a primary value of true. If the primary value is true, subsequent values returned by `fold-call` are those that should be substituted for the function call.

By default, `fold-call` returns false, so no function call is folded.

The multiple value return may be a bit confusing, so here are a few examples. If a client wants to fold `(integer-length 7)`, `fold-call` should return the values true and 3. For `(values 'a 3)`, `fold-call` should return true, A, and 3. It is done this way to allow clients to decide whether to fold based on arbitrary conditions other than having constant arguments, such as to preserve calls so that they can be traced. It also allows functions that return multiple values to be folded.

As with type derivers, clients should use the arguments provided to `fold-call` rather than examining the call instruction. Meta evaluation can and will pass `mv-call`s for folding.

## client transforms

Finally, meta evaluate can call out to the client to perform arbitrary specified transformations of calls. If meta evaluation reaches a call with an identity attribute, it will call the `transform-call` generic function. If the client has specialized a method to do so, `transform-call` can perform some arbitrary transformation on the BIR, then return true to indicate that it has done so (or false, if it chooses to do nothing). `transform-call` receives as arguments the system, the identity, and the call instruction.

This mechanism is intended to facilitate arbitrary transformations peculiar to particular functions. Very many transformations are possible, but a few examples might include: using specialized versions of functions based on type information, "constant folding" functions based on type information (e.g. `(plusp (the (integer 0) x))`), using machine integer arithmetic when only low bits are needed.

The utility of many of these transformations will depend on characteristics of the client's runtime and the rest of its compiler, and can additionally depend on policy decisions such as safety. As such, by default, `transform-call` does nothing and returns false.

The following design is recommended for custom transforms, and may be better facilitated in future versions. The transformation should use asserted (not necessarily derived) type information to decide what to do: for example `(length (the vector x))` might let a client decide to use a specialized vector length operator even if the type declaration has not been proven to hold. Then, the transformation should proceed by producing a new BIR function, transforming the call into a local call to that function, and then using the interpolator to incorporate the function's instructions into the caller. This allows transformations to not have to worry about argument evaluation order or the surrounding BIR. For type safety, the client should add type check `thei`s before using unsafe operators; meta evaluate will then move or eliminate these `thei`s if it can prove doing so is safe, and otherwise leave them in to prevent undefined behavior.

# Citations

Matthew Fluet and Stephen Weeks. Contification using dominators. ACM SIGPLAN Notices, 36:10 2, 2001. doi: 10.1145/507669.507639
