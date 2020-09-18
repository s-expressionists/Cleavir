"Block-based Intermediate Representation". Hopefully just "HIR" in the future though.

This is a new IR format for Cleavir based on my experienced problems with the original HIR format. It is intended to make it much easier to write transformations, while preserving HIR's goals, such as environment-independence.

Structure
=========

BIR programs are composed of instructions (class INSTRUCTION) and data (class DATUM). Instructions abstract activities the machine can carry out, while data abstract, well, data.

Instructions are organized into IBLOCKS which are themselves organized into FUNCTIONS. A given instruction has only one iblock it is in and an iblock has only one function it is in.

A function has a list of input ARGUMENTs (a type of datum, see below) and a set of IBLOCKs. It keeps track of its start iblock and its end iblock, if any. The start iblock is where control enters, and the end iblock is where it exits normally. If the function never exits normally (e.g. its body is a loop with no exit) the end will be nil to indicate this.

A IBLOCK is a straight-line sequence of INSTRUCTIONs. Each iblock keeps track of its start and end instructions, as well as its predecessors (local and not). It also has a DYNAMIC-ENVIRONMENT, more on that below.

It has an ordered list of input PHIs; anything transferring control to the iblock needs to pass VALUEs for these arguments. PHIs are used to indicate data flow confluence, from branches or `cl:block`. They do not keep track of their definitions directly, and instead indirect through their iblock's predecessor set; this way alterations to the control flow graph are automatically reflected by the phis without special coordination or a need to order predecessors.

IBLOCKs are called "iblocks" instead of "blocks" because shadowing `cl:block` could get more confusing than the other shadowing being done.

Note that IBLOCKs are not necessarilly basic blocks, i.e. a IBLOCK may unconditionally jump to another IBLOCK which has only it as a predecessor. This can happen both because of foibles during generation and for reasons relating to the dynamic environment.

An INSTRUCTION represents a thing the abstract machine can do. An instruction has zero or more inputs, and zero or more outputs, and all of them are data. Some instructions return a single datum, in which case they are called a COMPUTATION and identified with that datum. Instructions that aren't COMPUTATIONs are called OPERATIONs, and may output zero or more OUTPUTs, another subclass of DATUM.

For almost all instructions, all inputs and outputs are linear (see below). The exceptions are `readvar` and `writevar`, which input and output (respectively) a VARIABLE instead.

Instructions maintain links to their predecessor and successor, provided these are in the same block. They also maintain a link to their iblock.

An instruction that ends a IBLOCK is called a TERMINATOR, and maintains a sequence of IBLOCKs it can transfer control to. Inputs to a terminator may be passed to the next iblock, depending on the particular semantics of the terminator.

Data maintain sets of their definitions and uses, as well as a name for debugging purposes, and an rtype (explained below).

Data are divided into several classes depending on their meaning and property. The abstract subclass LINEAR-DATUM indicates a datum that is only used at one place in the program; this is a useful property of most intermediate computations. ARGUMENTs, PHIs, and OUTPUTs, already mentioned, are all linear. CONSTANTs, hopefully self-explanatory, are also linear. LINEAR-DATUM objects are defined and used in the same function and so cannot be shared between functions.

Lisp lexical variables correspond to the datum subclass VARIABLE. These can be written or read in any number of places, including across different functions. Each variable maintains links to its OWNER, the function where it is defined, and its BINDER, the dynamic environment that introduces its binding (e.g., a FUNCTION, usually).

VALUEs cannot be shared between functions, and all uses of a VALUE (i.e. instructions that have them as inputs) must be dominated by the VALUE's definition. To share data between functions, argument passing and shared VARIABLEs are the way to go.

Dynamic environments
--------------------

A DYNAMIC-ENVIRONMENT represents a Lisp dynamic environment, including information about exit points, dynamic-extent values, and unwind-protect cleanups. Any FUNCTION is a DYNAMIC-ENVIRONMENT, and in this capacity represents the dynamic environment the function was called in. Certain instructions are DYNAMIC-ENVIRONMENTs as well. Each iblock has a DYNAMIC-ENVIRONMENT and all instructions in that iblock conceptually share that dynamic environment. The localization of DYNAMIC-ENVIRONMENTs to iblocks means that a straight-line sequence of iblocks cannot be in general collapsed into a single basic block, because different instructions need different dynamic environments.

A client may or may not represent dynamic environments as concrete objects at runtime. This representation is intended to allow any implementation I can think of while making it hard to screw up important invariants, such as dynamic environments not being shareable between functions.

RTYPEs
------

Every DATUM has an RTYPE. An RTYPE represents the _static_ type of the value. This is a partly independent concern from its Lisp type. "Boxed" Lisp objects all have the rtype `:object`, and this is by far the most common rtype in most code. Unboxed lisp objects may have other rtypes, such as `:single-float`. `:multiple-values` is a distinguished rtype, and the `:continuation` rtype is used for the implicit object allocated to facilitate nonlocal `cl:return-from` and `cl:go`. Other than these, it is intended that clients will define the valid rtypes and when they are used.

Primops
-------

Many operations that have different instructions in the original HIR are subsumed under two instructions, `vprimop` and `nvprimop`. These represent "function-like" instructions, essentially those that take some inputs and return some outputs, with no other syntactic foibles. This covers e.g. `car`, `rplaca`, `standard-instance-access`, most arithmetic. I'm hoping this will reduce the proliferation of instruction classes in HIR. Each primop instruction just maintains a link to a structure describing the primop.

Constants, MAKE-LOAD-FORM, etc.
-------------------------------

Not designed yet.

Examples
========

BIR data is of course represented as Lisp objects rather than in a textual format. However, the organization into IBLOCKs makes a textual representation based on the objects fairly simple, so that's what's done here.

Coercion to multiple values
---------------------------

```
(lambda (x) x)

((function NIL start (%x)
  (start ()
   (:= (%0) (fixed-to-multiple %x))
   (returni %0))))
```

This isn't much different from HIR.

The `returni` instruction represents the normal return from a function. A function has at most one `returni` instruction.

Multiple values
---------------

```
(lambda (x y) (values x y))

((function NIL start (%x %y)
  (start ()
   (:= (%0) (fixed-to-multiple %x %y))
   (returni %0))))
```

Branch
------

```
(lambda (x y z) (if x y z))

((function NIL start (%x %y %z)
  (start ()
   (eqi %x 'nil (else then)))
  (then ()
   (:= (%r) (jump %y (ret))))
  (else ()
   (:= (%r) (jump %z (ret))))
  (ret (%r)
   (:= (%0) (fixed-to-multiple %r))
   (returni %0)))
```

This shows how multiple iblocks and passing between iblocks works. `eqi` tests whether the values `%x` and `'nil` are `eq`, i.e. whether the first argument is `nil`. If it is, control passes to the else iblock, and if not then to the then iblock. The `then` and `else` iblock unconditionally pass `%y` and `%z`, respectively, to the `ret` iblock, which uses its argument to carry out the normal return sequence.

Loop
----

```
(lambda (predicate f x)
  (loop until (cleavir-primop:funcall predicate x)
        do (setq x (cleavir-primop:funcall f x))))

((function NIL start (%predicate %f %x)
  (start ()
   (:= (%y) (jump %x (loop-begin))))
  (loop-begin (%y)
   (:= (%pv) (call %predicate %y))
   (:= (%pe) (multiple-to-fixed %pv))
   (eqi %pe 'nil (loop-next loop-finish)))
  (loop-next ()
   (:= (%fr) (call %f %y))
   (:= (%y) (jump %fr (loop-begin))))
  (loop-finish ()
   (:= (%r) (fixed-to-multiple %y))
   (returni %r))))
```

This demonstrates how PHI nodes work. The PHI %y is linked to its `loop-begin` block, and defined by the two jump instructions. `loop-finish` is obviously dominated by `loop-begin` so it is permitted to use `%y`.

Lexical variables
-----------------

```
(lambda (x) (lambda () x))

((function OUTER start (%x)
  (start ()
   (:= (X) (writevar %x))
   (:= (%r) (enclose INNER))
   (:= (%0) (fixed-to-multiple %r))
   (returni %0)))
 (function INNER start ()
  (start ()
   (:= (%y) (readvar X))
   (:= (%0) (fixed-to-multiple %y))
   (returni %0))))
```

You can see here that none of the temporary data are shared (the name `%0` is coincidental), data can be shared through the variable `X`. Note that this example takes place before cell conversion; closure conversion should, as in HIR, replace things with `make-cell` etc. instructions.

Nonlocal exit
-------------

```
(lambda (f x) (block nil (cleavir-primop:funcall f (lambda () (return x)))))

((function OUTER start (%f %x)
  (start ()
   (:= (X) (writevar %x))
   (:= (%continuation) (catch (normal end))))
  (normal ()
   (:= (CONTVAR) (writevar %continuation))
   (:= (%inner) (enclose INNER))
   (:= (%r) (call %f %inner))
   (:= (%v) (jump %r (end))))
  (end (%v)
   (returni %v)))
 (function INNER start ()
  (start ()
   (:= (%x) (readvar X))
   (:= (%0) (fixed-to-multiple %x))
   (:= (%c) (readvar CONTVAR))
   (unwind %c %1 (OUTER/end)))))
```

Here the iblock named `normal` (and only that iblock) has the `catch` instruction as its dynamic environment; not sure of a good way to indicate that textually yet.

The `unwind` instruction executes a nonlocal exit, passing a DATUM (here a `:multiple-values`) to its destination iblock as it does. A new variable CONTVAR is introduced to handle the shared continuation value. This is like how it works in HIR now, except that the variable is explicitly introduced before closure conversion so that closure conversion only has to look at variables, not all data.

Comparison with HIR
===================

BIR makes it possible to map over instructions etc. without consing. Transformations relating to control flow, e.g. finding basic blocks, should be much easier as comparatively few blocks, rather than every single instruction, needs to be looked at.

Since only the few lexical variables are shared between functions, closure conversion is substantially simplified.

BIR involves a frankly uglier ast-to-ir process, as it has to do more bookkeeping with iblocks and functions and so on. I'm hoping this can be somewhat abated if I take another look at it when it's not two in the morning. But I think I'm willing to tolerate some ugliness in AST-to-IR if it makes all the transformations less ugly.

Since BIR is relatively well-defined, I've already started on a verifier (in verify.lisp) to catch malformed IR. We have some things like this in HIR but they're ad-hoc, and important things like catching use-before-define are difficult to note when any datum might be shared or mutable.

Done so far
===========

Enough to translate a decent subset of lisp code into BIR

TODO
====

* Almost everything after initial HIR production
    * inlining, catch elimination, type inference
    * MIR/LIR: Pointer arithmetic, registers, etc
    * Machine code translation
  * Direct interpreter, for testing if nothing else
  * Handling literals/constants (pretty much the same issues as HIR)
  * Formal definition, or at least a stricter verifier
  * Multiple values might need rethinking (probably done)
  * Need to do some more bookkeeping, especially at AST-to-IR time
    when large swathes of instructions can be deleted without various
    backpointers being updated or a note being issued
