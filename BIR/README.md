"Block-based Intermediate Representation". Hopefully just "HIR" in the future though.

This is a new IR format for Cleavir based on my experienced problems with the original HIR format. It is intended to make it much easier to write transformations, while preserving HIR's goals, such as environment-independence.

Structure
=========

Modules
-------

The top level structure of BIR is the `module` class. A module is a set of functions being compiled together. Functions can "directly" call or otherwise refer only to other functions in the same module; all other accesses are indirect, e.g. through an environment with `cl:fdefinition`. The set of functions in a module can be accessed with `functions`.

Sets
----

The set is a data structure used throughout BIR. A set is an unordered collection of elements. Sets are used instead of lists when no ordering is required, as it allows some operations to be done more efficiently. The set data structure is defined and implemented by the `cleavir-set` subsystem and not further described here.

Functions
---------

The next highest unit of structure is the `function`. A `function` is the IR representation of a Lisp function being compiled.

Functions have the following accessible properties:

 * `lambda-list` represents the lambda list of the function. It is a list where each element is either a lambda list keyword, an `argument` (a required parameter), a list of two `argument`s (an optional parameter and suppliedp variable), or a list of a symbol and two `argument`s (a keyword, keyword parameter, and suppliedp variable).
 * `iblocks`
 * `start` is the iblock beginning the function.
 * `end` is the final iblock of the function, or `nil` if the function never returns normally. If it exists, the `end` iblock is the sole iblock in the function to end with a `returni` instruction.
 * `variables` is the set of variables bound by the function.
 * `environment` is the set of variables closed over by the function.
 * `origin`, `policy`, `name`, `docstring`, and `original-lambda-list` are carried over unchanged from the `function-ast` and are not used by the BIR subsystem.

Iblocks
-------

`iblock`s, short for "instruction blocks", are straight-line sequences of `instruction`s. Every iblock has zero or more non-terminator instructions, which represent some action that does not have usual control flow, and one final `terminator` instruction that indicates what `iblock`s control proceeds to after its execution.

Iblocks have the following accessible properties:

 * `inputs` is a sequence of `phi` data representing inputs to the iblock. Iblocks representing a control merge from `cl:block`, `cl:if`, or other operators have inputs, but most don't. Data are explained more below.
 * `start` is the start instruction of the iblock, and `end` the `terminator`.
 * `predecessors` is the set of iblocks in the same function that can transfer control to the given iblock. `entrances` is the set of iblocks in other functions that can transfer to this iblock by unwinding.
 * `dynamic-environment` (below)
 * `function` is the function this iblock belongs to.
 * `name` is used for debugging.

Instructions
------------

`instruction`s represent actions the machine can carry out. Each kind of action has its own subclass of `instruction`.

Instructions are divided into numerous subclasses and mixins. A `computation` is an instruction that returns a single `transfer` (explained below); in this case the instruction is identified with the datum and serves in both roles. Other instructions are `operation`s. `terminator`s are instructions that end an iblock.

Instructions have the following accessible properties:

 * `predecessor` and `successor` are the instructions immediately before or after an instruction. Alternately, instructions beginning an iblock have `nil` as their predecessor, and terminators have `nil` as their successor. Higher level control flow is instead at the iblock level.
 * `inputs` and `outputs` are sequences of data.
 * `iblock` is the iblock the instruction belongs to.
 * `policy` and `origin` are copied from the instruction's originating AST, and are not used by the BIR subsystem.

Terminators additionally have `next`, an ordered, possibly empty list of iblocks that they can transfer control to.

Data
----

Instances of the `datum` class represent arguments, intermediate results, return values, etc.

Every datum has a set of `definitions` and `uses`. Definitions "write" the datum's value and uses "read" them. Definitions and uses are generally instructions, except that some immutable data (e.g. constants) conceptually define themselves, and so have themselves as their definition. The sets of definitions and uses of a datum can be read with the `definitions` and `uses` functions respectively.

Data are divided into several different classes depending on their nature and role. An `ssa` is a datum that can only have one definition. A `linear-datum` is a datum that can only have one use. Both of these properties are useful for analysis. A `transfer` is both an `ssa` and a `linear-datum`. A `value` is an `ssa` that acts as its own definition.

Note that other kinds of datum may have only one definition or use without being actual members of the `ssa` or `linear-datum` classes; these classes just cover kinds of data that _necessarily_ have these properties.

Kinds of data include `argument`s to functions, `phi` nodes representing data confluence, `computation` instructions, `output`s of `operation` instructions, and `variable`s representing Lisp lexical variables. `argument`s and `constant`s are `value`s and `transfer`s. `phi`s are linear, but have multiple definitions. `output`s are `transfer`s, but not `value`s as they are defined by an operation. `variable`s may have any number of definitions and uses.

Additionally, `function`s are `value`s, as they are used as inputs to calls and encloses.

Every datum has the following accessible properties:

 * `name` is used for debugging.
 * `rtype` represents the "representation type" of the datum. This is it's "low level" type, not a lisp type specifier. Available rtypes are `:object`, representing "boxed" general Lisp objects; `:multiple-values`; and anything a client wants to define.

`phi`s have an `iblock`, which is the iblock they are a confluence at; that is to say, the phi's definitions are the terminators of the predecessors of its `iblock`.

`variable`s have a `binder`, which is the instruction that creates their binding. They also have an `extent` indicating whether they have dynamic or indefinite extent, for storage optimization purposes.

Data other than `phi`s and `variable`s cannot be shared between functions. That is, for other data, all definition and use instructions belong to the same function.

Dynamic environments
--------------------

A `dynamic-environment` represents a Lisp dynamic environment, including information about exit points, dynamic-extent values, and unwind-protect cleanups. Any `function` is a `dynamic-environment`, and in this capacity represents the dynamic environment the function was called in. Certain instructions are as well. Each iblock has a dynamic environment, and all instructions in that iblock conceptually share that dynamic environment. The localization of dynamic environments to iblocks means that a straight-line sequence of iblocks cannot be in general collapsed into a single basic block, because different instructions need different dynamic environments.

A client may or may not represent dynamic environments as concrete objects at runtime. This representation is intended to allow any implementation I can think of while making it hard to screw up important invariants, such as dynamic environments not being shareable between functions.

Primops
-------

Many operations that have different instructions in the original HIR are subsumed under two instructions, `vprimop` and `nvprimop`. These represent "function-like" instructions, essentially those that take some inputs and return some outputs, with no other syntactic foibles. This covers e.g. `car`, `rplaca`, `standard-instance-access`, most arithmetic. I'm hoping this will reduce the proliferation of instruction classes in HIR. Each primop instruction just maintains a link to a structure describing the primop.

Instruction classes
===================

enclose
-------

Represents the creation of a Lisp function object, packaging together
its `function` and environment.

unreachable
-----------

Indicates that control cannot possibly reach this point. Effects are undefined if it does.

writevar, readvar
-----------------

Write/read a variable's value.

vprimop, nvprimop, tprimop
--------------------------

Superclass `primop`. Represent `computation`, `operation`, and `terminator` primops respectively. At this time there is no class for a terminator primop which also outputs values.

call, mv-call, local-call
-------------------------

Superclass `abstract-call`. Represents a function call. For the second, all inputs have a `:multiple-values` rtype. The callee input can be obtained with the `callee` function.

For the first two, the callee is a datum representing a Lisp function, e.g. the output of an `enclose`. For the last the callee is a BIR `function` being called "directly".

returni
-------

Return values from a function.

alloca
------

Allocate storage with dynamic extent.

writetemp, readtemp
-------------------

Write/read some `alloca` storage's value.

catch
-----

Mark a nonlocal entrance point. It can be closed over and used by the
`unwind` instructions to find the correct stack frame. `unwind`s of a
catch returns a set of all unwinds using the catch.

leti, dynamic-leti
------------------

Bind variables. For the latter, dynamic extent is explicitly marked.

unwind
------

Perform a nonlocal exit. Inputs are passed to the `phi`s of the
destination iblock.

jump
----

Perform a local control transfer. Inputs are passed to the `phi`s of the destination iblock.

eqi
---

Determine if two inputs are `cl:eq` or not. Jump to the first iblock if they are and the second otherwise.

typeq
-----

Test the type of an object.

case
----

Branch based on an input's `eq`-ness to several other objects.

fixed-to-multiple
-----------------

Construct a multiple-values from a sequence of values.

multiple-to-fixed
-----------------

Extract the values from a multiple-values datum.

cast
----

Cast a datum of one rtype to another rtype.

Examples
========

BIR is of course represented as Lisp objects rather than in a textual format. However, the organization into `iblock`s makes a textual representation based on the objects fairly simple, so that's what's done here.

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
   (catch <nil> (normal end)))
  (normal ()
   (:= (%inner) (enclose INNER))
   (:= (%r) (call %f %inner))
   (:= (%v) (jump %r (end))))
  (end (%v)
   (returni %v)))
 (function INNER start ()
  (start ()
   (:= (%x) (readvar X))
   (:= (%0) (fixed-to-multiple %x))
   (unwind <nil> %1 (OUTER/end)))))
```

Here the iblock named `normal` (and only that iblock) has the `catch` instruction as its dynamic environment; not sure of a good way to indicate that textually yet.

The `unwind` instruction executes a nonlocal exit, passing a DATUM (here a `:multiple-values`) to its destination iblock as it does.

Comparison with HIR
===================

BIR makes it possible to map over instructions etc. without consing. Transformations relating to control flow, e.g. finding basic blocks, should be much easier as comparatively few blocks, rather than every single instruction, needs to be looked at.

Since only the few lexical variables are shared between functions, closure conversion is substantially simplified.

BIR involves a frankly uglier ast-to-ir process, as it has to do more bookkeeping with iblocks and functions and so on. I'm hoping this can be somewhat abated if I take another look at it when it's not two in the morning. But I think I'm willing to tolerate some ugliness in AST-to-IR if it makes all the transformations less ugly.

Since BIR is relatively well-defined, I've already started on a verifier (in verify.lisp) to catch malformed IR. We have some things like this in HIR but they're ad-hoc, and important things like catching use-before-define are difficult to note when any datum might be shared or mutable.

TODO
====

* Almost everything after initial HIR production
    * type inference
    * MIR/LIR: Pointer arithmetic, registers, etc
  * Direct interpreter, for testing if nothing else
