This is a new IR format for Cleavir based on my experienced problems with the original HIR format. It is intended to make it much easier to write transformations, while preserving HIR's goals, such as environment-independence.

Basic structure
===============

BIR has several levels of structure. The classes used to describe this structure are mostly defined in structure.lisp.

At the top level is the `module`. A module is a set of functions being compiled together. Functions can directly call or otherwise refer only to other functions in the same module. All other accesses are indirect, e.g. through an environment via `cl:fdefinition`.

The next level down is the `function`. A function is the IR representation of a Lisp function being compiled. It has a lambda list, like the Lisp function, and some iblocks, representing the compiled code.

`iblock`s, short for "instruction blocks", are straight-line sequences of `instruction`s. Every iblock has zero or more non-terminator instructions, which represent some action that has straightforward control flow, and one final `terminator` instruction that indicates what `iblock`s control proceeds to after its execution. In other words, an iblock is a basic block.

`instruction`s represent actions the machine can carry out. Each kind of action has its own subclass of `instruction`. Most instruction classes are defined in instructions.lisp, and clients may define their own instructions as well.

A broad subset of instructions has "function-like" semantics, in that they have one or more inputs that are evaluated normally, and possibly return a result. These "primops" are not actually functions, and can probably be dealt with by the code generator directly (i.e. using some stereotyped sequence of machine instructions rather than an actual function call). In BIR, primops are represented with the `primop` instruction, which links to the specific `primop-info` of the primop. These are the primop info structures in the Cleavir/Primop/ directory.

Instructions input and output data, of the `datum` class. A datum represents zero or more values that can exist at runtime. In some cases, these data may not correspond directly to Lisp objects. There are several subclasses of `datum` based on properties and role. Most data have strong restrictions on their use in order to facilitate analysis and optimization. `ssa` (single static assignment) data are assigned in at most one place, and `linear-datum` are used in at most one place, for example.

Although data can represent any number of values, AST-to-BIR is set up so that only one datum of unknown values is really live at any given time. If multiple unknown values data need to be alive due to the semantics of the source program, e.g. from `multiple-value-prog1`, one set of values will be explicitly saved and restored in the BIR. This is intended to facilitate a BIR code generator being able to map BIR data simply to registers or memory locations.

A `dynamic-environment` represents a Lisp dynamic environment, including information about exit points, dynamic-extent values, and unwind-protect cleanups. Any `function` is a `dynamic-environment`, and in this capacity represents the dynamic environment the function was called in. Certain instructions are dynamic environments. Every dynamic environment except a function has a parent dynamic environment, and this chain of parents will eventually reach the function. Dynamic environments are never shared between functions. Each iblock has a dynamic environment, and all instructions in that iblock conceptually share that dynamic environment. The localization of dynamic environments to iblocks means that a straight-line sequence of iblocks cannot be in general collapsed into a single basic block, because different instructions need different dynamic environments. A client may or may not represent dynamic environments as concrete objects at runtime. This representation is intended to allow any implementation I can think of while making it hard to screw up important invariants, such as dynamic environments not being shareable between functions.

Analysis and transformations
============================

BIR has several properties intended to allow analyses and optimizations to be expressed simply and efficiently. Analysis passes may look through the IR and store information elsewhere, or in the IR itself. Optimization passes may transform the IR to new more efficient forms. Cleavir's own optimization passes mostly live in Cleavir/BIR-transformations/.

Most data have only one definition and one use: function `argument`s, and instruction `output`s meet this condition. Data with only one use - instances of `linear-datum`, which include arguments, outputs, as well as `phi` nodes used in conditionals and other control merges - necessarily have that use tied to a specific control point, that use, so forward-propagated information can be associated with them directly. The two main kinds of forward-propagated data at the moment are type information and _attributes_; the former are types in the Lisp sense (sets of objects), while the latter are described in Cleavir/Attributes.

Functions may be called through a closure, or directly with _local calls_, described below. Calls of the first kind are difficult to analyze in two directions: the caller knows nothing about the callee, so it can't rely on its behavior, while the callee knows nothing about how it is called, so it must accept any input. Local calls are much easier to analyze, and can be treated as control flow, or in some cases outright removed as the locally called function is inlined. Marking as many calls as possible as local is important to ensure they can be analyzed effectively.

BIR can be mapped over efficiently with the functions and macros in map.lisp. These operators generally work without consing and in forward flow order, using internally maintained sequential lists.

Verifier
========

The BIR verifier function `verify`, defined in verify.lisp, checks that a given module maintains many invariants necessary for BIR to be well-formed. Compiler writers should use `verify` after passses if they are unsure that the pass's transformation is valid. Note that the verifier does not prove any kind of "correctness" of the Lisp code the BIR represents; a verification failure merely indicates a bug in Cleavir's BIR handling, or in a client's pass.

`verify` returns silently if no problems were encountered. If the BIR is invalid, however, it will print a disassembly of the module and a listings of what conditions it found were violated.

Disassembler
============

The BIR disassembler prints a textual representation of BIR for use in debugging compilers. The main entry point is `cleavir-bir-disassembler:display`. This function can be used on multiple levels of BIR structure, from a module down to an instruction, for when only part of a module needs to be displayed. The disassembler output will look like that in the examples below.

Examples
========

This section briefly describes how BIR represents various examples of Lisp code.

Identity function
-----------------

The identity function `(defun identity (x) x)` can be represented as

```
-------module-------
constants: ()
function IDENTITY (x)
     with environment ()
     with start iblock IDENTITY-START
  iblock IDENTITY-START ():
   dynenv = (FUNCTION IDENTITY)
     (leti x) -> X
     (readvar x) -> x-0
     (returni x-0)
```

The function has no constants, only one function in its module, and only one iblock in that function. The iblock merely binds a `variable` datum X, immediately reads from it, and returns the result. The reason for the variable binding is that in BIR, function `argument`s are linear SSA data, i.e. may only be assigned once and used once, so in general an argument will need to be bound to an argument to allow Lisp code to work with it. In this case, however, there is only one binding and one read for the variable, so it could be deleted, and the `returni` can simply return the argument directly.

Calls, constants
----------------

`(lambda (x) (foo x) (foo x))` might be represented as

```
-------module-------
constants: (FOO)
function (LAMBDA (X)) (x)
     with environment ()
     with start iblock (LAMBDA (X))-START
  iblock (LAMBDA (X))-START:
   dynenv = (FUNCTION (LAMBDA (X)))
     (leti x) -> X
     (constant-reference 'foo) -> 0
     (primop 0 fdefinition) -> FOO
     (readvar x) -> X-0
     (call foo x-0) -> 1
     (readvar x) -> X-1
     (call foo x-1) -> 2
     (returni 2)
```

The function is unknown to the compiler, so it is looked up at runtime using the `fdefinition` primop. It is then called twice.

Note that there are two `readvar` instructions. This is because the X variable is exceptional in having any number of writes and reads. A `readvar` instruction must exist for each different usage, even here where there is no actual need to perform any kind of new lookup operation at runtime. Similarly, a constant may be referenced any number of times, but there must be one `constant-reference` instruction for each usage.

Control flow
------------

Now consider a non-straight-line function,

```lisp
(lambda (predicate f x)
  (loop until (cleavir-primop:funcall predicate x)
        do (setq x (cleavir-primop:funcall f x))))
```

Here `cleavir-primop:funcall` is used instead of `funcall` to avoid code to resolve function designators.

```
-------module-------
constants: (NIL)
function (LAMBDA (PREDICATE F X)) (predicate f x)
     with environment ()
     with start iblock (LAMBDA (PREDICATE F X))-START
  iblock (LAMBDA (PREDICATE F X))-START ():
   dynenv = (FUNCTION (LAMBDA (PREDICATE F X)))
     (leti x) -> X
     (jump (tag-next-loop))
  iblock TAG-NEXT-LOOP ():
   dynenv = (FUNCTION (LAMBDA (PREDICATE F X)))
     (readvar x) -> X-1
     (call predicate x-1) -> 4
     (ifi 4 (if-then if-else))
  iblock IF-ELSE ():
   dynenv = (FUNCTION (LAMBDA (PREDICATE F X)))
     (readvar x) -> X-0
     (call f x-0) -> 2
     (writevar 2) -> X
     (jump (tag-next-loop))
  iblock IF-THEN ():
   dynenv = (FUNCTION (LAMBDA (PREDICATE F X)))
     (constant-reference 'nil) -> 0
     (returni 0)
```

The function now has four iblocks: the beginning and loop pre-header `(LAMBDA (PREDICATE F X))-START`, the loop header `TAG-NEXT-LOOP`, another in-loop iblock `IF-ELSE` in which the function `f` is called, and the exit block `IF-THEN`. Control starts at `(LAMBDA (PREDICATE F X))-START`, and then proceeds unconditionally (using the `jump` instruction) to `TAG-NEXT-LOOP`. `TAG-NEXT-LOOP` calls the predicate, and tests its result with `ifi`: if true `ifi` jumps to `IF-THEN`, and otherwise `IF-ELSE`. `IF-ELSE` calls `f`, stores its result in the variable `X`, then jumps back to the top of the loop. `IF-THEN` simply returns nil from the function.

`ifi` is the main branch instruction in BIR, and is used for almost all conditionals (the exception being multiway branches, which are more arcane). It has similar semantics to `cl:if`, jumping to either the "then" or "else" block based on its input being `nil` or not. However some `conditional-test` instructions have no meaning except for outputting to `ifi`, and in this case, the `ifi` may be compiled to branch directly on a condition without a boolean value actually existing at runtime. In this case, the `ifi`'s input is just a boolean value, so this is not done.

Local functions
---------------

The Lisp function `(lambda (x) (flet ((foo () x)) (values (foo) #'foo)))` could be represented, after a few optimizations, as

```
-------module-------
constants: ()
function (LAMBDA (X)) (x)
     with environment ()
     with start iblock (LAMBDA (X))-START
  iblock (LAMBDA (X))-START:
   dynenv = (FUNCTION (LAMBDA (X)))
     (leti x) -> X
     (enclose (flet foo)) -> (FLET FOO)-0
     (local-call (flet foo)) -> 0
     (fixed-to-multiple 0 (FLET FOO)-0) -> 4
     (returni 4)
function (FLET FOO) (x)
     with environment (x)
     with start iblock (FLET FOO)-START
  iblock (FLET FOO)-START:
   dynenv = (FUNCTION (FLET FOO))
     (readvar x) -> x-0
     (returni x-0)
```

The `enclose` instruction produces a Lisp closure from a function. The difference, here, is that a function is just code, whereas a closure is a Lisp object that has the code as well as anything it closes over.

The difference is demonstrated by `(local-call (flet foo))`. A local call is a call to another function in the same module - here `(flet foo)`. The function is called directly, rather than the closure being called. Such a call can be dealt with more directly - for example, a closure may not need to be allocated for the function if it is only ever locally called, analysis can use information about the exact code being called, and the code generator may have unrestrained ability to choose a convenient calling convention.

The variable `X` is shared between the two functions in the module, with the anonymous function binding it, and `foo` reading from it (and having it in its environment).

Nonlocal exit
-------------

`(lambda (f x) (block nil (cleavir-primop:funcall f (lambda () (return x)))))` could be

```
-------module-------
constants: ()
function (LAMBDA (F X)) (f x)
     with environment ()
     with start iblock (LAMBDA (F X))-START
  iblock (LAMBDA (F X))-START:
   dynenv = (FUNCTION (LAMBDA (F X)))
     (leti x) -> X
     (catch tag (block-nil block-nil-merge))
  iblock BLOCK-NIL ():
   dynenv = (CATCH IN (LAMBDA (F X))-START)
     (enclose (lambda ())) -> (LAMBDA ())-0
     (call f (lambda ())-0) -> 1
     (jump 1 block-nil-merge)
  iblock BLOCK-NIL-MERGE (0):
   dynenv = (FUNCTION (LAMBDA (F X)))
   entrances = ((lambda ())-start)
     (returni 0)
function (LAMBDA ()) ()
     with environment (tag x)
     with start iblock (LAMBDA ())-START
  iblock (LAMBDA ())-START:
   dynenv = (FUNCTION (LAMBDA ()))
     (readvar x) -> x-0
     (unwind x-0 tag block-nil-merge) -> 0
```

The outer function, `(lambda (f x))`, binds `x` and then uses the `catch` instruction. This instruction indicates a nonlocal entrance point, i.e. a point another function can immediately exit to using `return-from`, `go`, etc. The outer function then proceeds to `BLOCK-NIL`, which creates a closure and calls `f` with this closure. If this call returns normally, the result is passed to `BLOCK-NIL-MERGE`.

The inner function simply reads the variable `x`, and then uses the `unwind` instruction. This performs a nonlocal exit. The second input to the `unwind` instruction, `tag`, represents the nonlocal entrance at the catch, and may have to exist at runtime as a sort of closure variable representing the target stack frame. `unwind` passes its first input, the read variable, to `BLOCK-NIL-MERGE` in the outer function.

`BLOCK-NIL-MERGE` simply returns its input, whether it's received from the outer function, or a nonlocal exit from the inner function. The input `0` is a phi node that is only used once, but which can be assigned by multiple `jump` and `unwind` instructions.
