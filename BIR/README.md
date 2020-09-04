"Block-based Intermediate Representation". Hopefully just "HIR" in the future though.

This is a new IR format for Cleavir based on my experienced problems with the original HIR format. It is intended to make it much easier to write transformations, while preserving HIR's goals, such as environment-independence.

Structure
=========

A BIR program is organized into FUNCTIONs. A function has a list of input ARGUMENTs (see below) and a set of IBLOCKs. It keeps track of its start iblock and its end iblock, if any. The start iblock is where control enters, and the end iblock is where it exits normally.

A IBLOCK is a straight-line sequence of INSTRUCTIONs. Each iblock keeps track of its start and end instructions, as well as its predecessors (local and not). It also has a DYNAMIC-ENVIRONMENT, more on that below. It has an ordered list of input ARGUMENTs; anything transferring control to the iblock needs to pass VALUEs for these arguments. (This is used instead of e.g. phi nodes.) IBLOCKs are called "iblocks" instead of "blocks" because shadowing `cl:block` could get more confusing than the other shadowing being done. At the moment there is no backpointer from an iblock to the function it's in.

Note that IBLOCKs are not necessarilly basic blocks, i.e. a IBLOCK may unconditionally jump to another IBLOCK which has only it as a predecessor. This can happen both because of foibles during generation and for reasons relating to the dynamic environment.

An INSTRUCTION represents a thing the abstract machine can do. An instruction has zero or more inputs, all of which are VALUEs. Some instructions return a single VALUE, in which case they are called a COMPUTATION. Instructions that aren't COMPUTATIONs are called OPERATIONs. At the moment there are no backpointers from instructions to the iblock or function they are in.

An instruction that ends a IBLOCK is called a TERMINATOR, and maintains a sequence of IBLOCKs it can transfer control to. Inputs to a terminator may be passed to the next iblock, depending on the particular semantics of the terminator.

A VALUE represents a runtime value. It has a single definition in the program and maintains its identity. Each value maintains a set of users (i.e. instructions that have it as an input) to facilitate transformations.

There are three kinds of VALUEs: COMPUTATIONs already mentioned, ARGUMENTs, and CONSTANTs. A constant is, well, a constant. An ARGUMENT represents a value passed to a function or iblock.

Lisp lexical variables correspond to objects called VARIABLEs. These are not VALUEs, because they can be written to multiple times. Instead, a program uses instructions WRITEVAR and READVAR to write and read the value of a variable.

VALUEs cannot be shared between functions, and all uses of a VALUE (i.e. instructions that have them as inputs) must be dominated by the VALUE's definition. To share data between functions, argument passing and shared VARIABLEs are the way to go.

Dynamic environments
--------------------

A DYNAMIC-ENVIRONMENT represents a Lisp dynamic environment, including information about exit points, dynamic-extent values, and unwind-protect cleanups. Any FUNCTION is a DYNAMIC-ENVIRONMENT, and in this capacity represents the dynamic environment the function was called in. Certain instructions are DYNAMIC-ENVIRONMENTs as well. Each iblock has a DYNAMIC-ENVIRONMENT and all instructions in that iblock conceptually share that dynamic environment. The localization of DYNAMIC-ENVIRONMENTs to iblocks means that a straight-line sequence of iblocks cannot be in general collapsed into a single basic block, because different instructions need different dynamic environments.

A client may or may not represent dynamic environments as concrete objects at runtime. This representation is intended to allow any implementation I can think of while making it hard to screw up important invariants, such as dynamic environments not being shareable between functions.

RTYPEs
------

Every VALUE has an RTYPE. An RTYPE represents the _static_ type of the value. This is a partly independent concern from its Lisp type. "Boxed" Lisp objects all have the rtype `:object`, and this is by far the most common rtype in most code. Unboxed lisp objects may have other rtypes, such as `:single-float`. `:multiple-values` is a distinguished rtype.

An "aggregate" rtype is an ordered sequence of rtypes. Aggregates can be used in order for a single VALUE to represent multiple Lisp values; e.g. a (currently hypothetical) TRUNCATE-FIXNUM instruction might have an rtype of `#(:fixnum :fixnum)`.

Constants, MAKE-LOAD-FORM, etc.
-------------------------------

Not designed yet.

Examples
========

BIR data is of course represented as Lisp objects rather than in a textual format. However, the organization into IBLOCKs makes a textual representation based on the objects fairly simple, so that's what's done here.

"%x" is an argument or computation. "'3" is a constant. "FUNCTION (...) foo 'bar" indicates the beginning of a function with the arguments in the parentheses that starts at iblock foo, and optionally named bar. "foo (...):" indicates the start of an iblock with the given arguments. An indented line is an instruction. "%x := ..." is a computation, while an operation will not have the pseudo-assignment.

Coercion to multiple values
---------------------------

```
(lambda (x) x)

FUNCTION (%x) start
start (%x):
  %0 := create #(:object) %x
  %1 := fixed-to-multiple %0
  returni %1
```

To make a single object into multiple values, it is first made into a single-unit aggregate, and then that is passed to the `fixed-to-multiple` instruction. `create #(:object)` means a value with that rtype is created.

This two-step process is used so that aggregates can be used without breaking them down. For example an implementation of `floor` using the hypothetical `truncate-fixnum` could just pass its aggregate result to `fixed-to-multiple` directly.

The `returni` instruction represents the normal return from a function. A function has at most one `returni` instruction.

Multiple values
---------------

```
(lambda (x y) (values x y))

FUNCTION (%x %y) start
start (%x %y):
  %0 := create #(:object) %x %y
  %1 := fixed-to-multiple %0
  returni %1
```

Branch
------

```
(lambda (x y z) (if x y z))

FUNCTION (%x %y %z) start
start (%x %y %z):
  eqi %x 'nil {else then}

then ():
  jump %y {ret}

else ():
  jump %z {ret}

ret (%r):
  %0 = create #(:object) %r
  %1 = fixed-to-multiple %0
  returni %1
```

This shows how multiple iblocks and passing between iblocks works. `eqi %x 'nil {then else}` tests whether the values `%x` and `'nil` are `eq`, i.e. whether the first argument is `nil`. If it is, control passes to the else iblock, and if not then to the then iblock. The `then` and `else` iblock unconditionally pass `%y` and `%z`, respectively, to the `ret` iblock, which uses its argument to carry out the normal return sequence.

Loop
----

```
(lambda (predicate f x)
  (loop until (cleavir-primop:funcall predicate x)
        do (setq x (cleavir-primop:funcall f x))))

FUNCTION (%predicate %f %x) start
start (%predicate %f %x):
  jump %x {loop-begin}

loop-begin (%y):
  %pv := call %predicate %y
  %pe := multiple-to-fixed %pv
  %p := extract %pe [0]
  eqi %p 'nil {loop-next loop-finish}

loop-next ():
  %fr := call %f %y
  jump %y {loop-begin}

loop-finish ():
  %a = create #(:object) %y
  %r = fixed-to-multiple %a
  returni %r
```

This demonstrates how the SSA-ness of VALUEs is maintained with looping. The introduced loop argument `%y` is defined solely by the `loop-begin` iblock. `loop-finish` is obviously dominated by `loop-begin` so it is permitted to use `%y`.

`extract %pe [0]` is a computation to extract the 0th element of the aggregate `%pe`, i.e. the primary value.

Lexical variables
-----------------

```
(lambda (x) (lambda () x))

FUNCTION (%x) start0 'OUTER
start0 (%x):
  writevar %x [X]
  %r := enclose [INNER]
  %0 := create #(:object) %r
  %1 := fixed-to-multiple %0
  returni %1

FUNCTION () start1 'INNER
  %y := readvar [X]
  %0 := create #(:object) %y
  %1 := fixed-to-multiple %0
  returni %1
```

You can see here that while none of the VALUEs are shared (the names of `%0` and `%1` are coincidental), data can be shared through the variable `X`. Note that this example takes place before closure conversion; closure conversion should, as in HIR, replace things with `make-cell` etc. instructions.

Nonlocal exit
-------------

```
(lambda (f x) (block nil (cleavir-primop:funcall f (lambda () (return x)))))

FUNCTION (%f %x) start0 'OUTER
start0 (%f %x):
  writevar %x [X]
  %continuation := catch {normal end}

normal ():
  writevar %continuation [CONTVAR]
  %inner = enclose [INNER]
  %r = call %f %inner
  jump %r {end}

end (%v):
  returni %v

FUNCTION () start1 'INNER
start1 ():
  %x = readvar [X]
  %0 = create #(:object) %x
  %1 = fixed-to-multiple %0
  %c = readvar [CONTVAR]
  unwind %c %1 [OUTER/end]
```

Here the iblock named `normal` (and only that iblock) has the `catch` instruction as its dynamic environment; not sure of a good way to indicate that textually yet.

The `unwind` instruction executes a nonlocal exit, passing a VALUE (here a `:multiple-values`) to its destination iblock as it does. A new variable CONTVAR is introduced to handle the shared continuation value. This is like how it works in HIR now, except that the variable is explicitly introduced before closure conversion so that closure conversion only has to look at variables, not VALUEs.

Comparison with HIR
===================

BIR makes it possible to map over instructions etc. without consing. Transformations relating to control flow, e.g. finding basic blocks, should be much easier as comparatively few blocks, rather than every single instruction, needs to be looked at.

Since only the few lexical variables are shared between functions, closure conversion should be substantially simplified. I've already written a BIR version of `build-function-dag`. I've also written a simple pass that eliminates VARIABLEs if they are only written to once. This should make transformations relating to data flow, i.e. most transformations, much simpler and more efficient, since they know that VALUEs only have one definition, and the relatively few mutable VARIABLEs are explicitly marked.

BIR involves a frankly uglier ast-to-ir process, as it has to do more bookkeeping with iblocks and functions and so on. I'm hoping this can be somewhat abated if I take another look at it when it's not two in the morning. But I think I'm willing to tolerate some ugliness in AST-to-IR if it makes all the transformations less ugly.

Since BIR is relatively well-defined, I've already started on a verifier (in verify.lisp) to catch malformed IR. We have some things like this in HIR but they're ad-hoc, and important things like catching use-before-define are difficult to note when any datum might be shared or mutable.

Done so far
===========

Enough to translate a decent subset of lisp code into BIR

TODO
====

  * Several instructions
    * fdefinition
    * type stuff (the, typeq, typew)
    * unwind-protect... is probably client-dependent like in HIR
    * data structures other than conses
  * Almost everything after initial HIR production
    * Closure conversion, inlining, catch elimination, type inference
    * MIR/LIR: Pointer arithmetic, registers, etc
    * Machine code translation
  * Direct interpreter, for testing if nothing else
  * Handling literals/constants (pretty much the same issues as HIR)
  * Formal definition, or at least a stricter verifier
