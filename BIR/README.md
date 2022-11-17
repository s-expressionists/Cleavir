Block-based Intermediate Representation (BIR) is Cleavir's primary intermediate representation. BIR is designed to represent both high-level, implementation-independent properties of Lisp programs, as well as lower-level implementation-defined machine-dependent properties, and to allow the compiler to make the transition from high to low level smoothly and efficiently. Pervasive use of CLOS facilitates extension and customization by clients.

# Table of Contents

1. [Structure of the IR](#structure_of_the_ir)
    1. [Data](#data)
        1. [Linearity](#linearity)
        2. [Multiple values](#multiple_values)
    2. [Primops](#primops)
    3. [Local calls](#local_calls)
    4. [Dynamic environments](#dynamic_environments)
        1. [Nonlocal exits](#nonlocal_exits)
2. [Analyses and transformations](#analyses_and_transformations)
3. [Verifier](#verifier)
4. [Disassembler](#disassembler)
5. [Examples](#examples)
    1. [Identity function](#identity_function)
    2. [Calls, constants](#calls,_constants)
    3. [Control flow](#control_flow)
    4. [Local functions and calls](#local_functions_and_calls)
    5. [Nonlocal exit](#nonlocal_exit)
6. [System Information](#system)
7. [Index](#index)

# Structure of the IR

BIR has several levels of structure. The classes used to describe this structure are mostly defined in `structure.lisp`.

At the top level is the `module`. A module is a set of functions being compiled together. Functions can directly call or otherwise refer only to other functions in the same module. All other accesses are indirect, e.g. through a global environment via `cl:fdefinition`.

The next level down is the `function`. A function is the IR representation of a Lisp function being compiled. It has a lambda list, like the Lisp function, and some `iblock`s, representing the compiled code.

`iblock`s, short for "instruction blocks", are straight-line sequences of `instruction`s. Every iblock has zero or more non-terminator instructions, which represent some action that has straightforward control flow, and one final `terminator` instruction that indicates what `iblock`s control proceeds to after its execution. In other words, an iblock is a basic block.

`instruction`s represent actions the machine can carry out. Each kind of action has its own subclass of `instruction`. Most instruction classes are defined in instructions.lisp, and clients may define their own instructions as well.

## Data

Instructions input and output "data", of the `datum` class. A datum represents zero or more values that can exist at runtime. In some cases, these data may not correspond directly to Lisp objects. There are several subclasses of `datum` based on properties and role. Most data have strong restrictions on their use in order to facilitate analysis and optimization. `ssa` (single static assignment) data are assigned in at most one place, and `linear-datum` are used in at most one place, for example.

### Linearity

One of the most important properties of data is _linearity_, in the sense of [linear logic](https://en.wikipedia.org/wiki/Linear_logic). A `linear-datum` is one that is only used in one place in the program. This is closely dual to the `ssa`, a datum that is defined in only one place. Most BIR data are linear, and this is critical for performant analysis.

To see why, consider the code

```lisp
(loop for x = ...
      do (if (typep x 'single-float)
             (print (- (* x 3.9) 7.2))
             (return x)))
```

Here, in `(* x 3.9)`, `x` is clearly a single float, and so this multiplication may be safely transformed into a fast machine operation. But just as clearly, in `(return x)` `x` is _not_ a single float, and in `(typep x 'single-float)` it could be anything. Therefore, for a variable `datum` like `x` that can be used in any number of places, the compiler needs to associate type information (as well as any other forward-flow information) with a `datum` _and_ a control point, not just a `datum`.

For a `linear-datum`, however, forward-flow information can be associated directly with the `datum`. The implicit meaning of this is that the information is associated to the datum and to the control point at which it is used. In this example, the compiler will construct a `linear-datum` (an `output`, specifically) for the result of the `(* x 3.9)` call. This `output` is only used in one place, the call to `-`. So Cleavir can tag that `output` as having type `single-float` without having to worry about control flow.

Linearity is pervasive: almost all BIR instructions only accept linear data. In this example, the BIR would include a special `readvar` instruction that takes the (non-linear) `variable` datum for `x` as input, and outputs a linear datum (an `output`) with its value. This `output` would then serve as the input to the `*` call.

Note that linear data can be used multiple times in any execution of the program - e.g. here, the result datum for the `*` call will be read once for each execution of the loop that reaches this point. The important thing is that the datum is used only once in the program text.

### Multiple Values

Although a `datum` can represent any number of values, BIR's semantics are such that only the primary value is retained after the values are produced, unless the `datum` is immediately input into the few instructions that use all values, such as `mv-call`. This reflects a usual implementation of multiple values, in which only one set of them is really live at any given time in most circumstances. If multiple multiple values are live simultaneously in the source program, e.g. in `cl:multiple-value-prog1`, one set of values will be explicitly saved and restored in the BIR by special instructions. This facilitates code generators being able to straightforwardly map BIR data to registers or memory locations.

## Primops

A broad subset of instructions has "function-like" semantics, in that they have one or more inputs that are evaluated normally, and possibly return a result. These "primops" are not actually functions, and can probably be dealt with by the code generator directly (i.e. using some stereotyped sequence of machine instructions rather than an actual function call). In BIR, primops are represented with the `primop` instruction, which links to the specific `primop-info` of the primop. These are the primop info structures in the Cleavir/Primop/ directory.

Primops will for the most part be produced from calls by transformations, rather than being present in the initial BIR. This is in order to improve modularity: a frontend can produce BIR while being disinterested in the particular nature of the backend, while client transforms on the BIR can cooperate with the client's backend to "lower" the IR to a more efficient but less general form.

## Local calls

BIR has a concept of "local" calls. A call is local if it is to another function in the same compilation module. Local calls are more useful to the compiler because it means the caller can "know" about the callee, and vice versa. Marking as many calls as possible as local is important to ensure they can be analyzed effectively.

For example, if a function is called non-locally, it must be prepared to accept any input; but if it's only called locally, it can be compiled to only accept whatever inputs are actually provided to those local calls. If we have `(flet ((f (x) (... (car x) ...))) (f (cons ...)))`, `f` may safely use inline `car` operations without type checking.

## Dynamic environments

A `dynamic-environment` represents a Lisp dynamic environment, including information about exit points, values with only dynamic extent, and unwind-protect cleanups. Any `function` is a `dynamic-environment`, and in this capacity represents the dynamic environment the function was called in. Certain instructions are also dynamic environments.

Every dynamic environment except a function has a parent dynamic environment, and this chain of parents will eventually reach the function. Each iblock has a dynamic environment, and all instructions in that iblock conceptually share that dynamic environment. The localization of dynamic environments to iblocks means that a straight-line sequence of iblocks cannot be in general collapsed into a single basic block, because different instructions need different dynamic environments.

One consequence of this is that any jump between iblocks may involve complex operations, as dynamic environments are unbound. This can include unbinding dynamic variables, invalidating exit points, and evaluating `cl:unwind-protect` cleanup code. Code generators must be careful to ensure all such "unwinding" operations will be carried out.

A client may or may not represent dynamic environments as concrete objects at runtime. BIR dynamic environments are intended to allow a variety of runtime realizations while maintaining static invariants, such as dynamic environments never being shared between functions.

### Nonlocal exits

Dynamic environments are used to represent nonlocal exit points, i.e. `cl:block`s and `cl:tagbody`s that are jumped to from an inner function. (Local exits are simple jumps.) A point that can be nonlocally exited to is marked by a `come-from` instruction.

The `come-from` appears in the BIR at the point at which the `cl:block` or `cl:tagbody` is entered, and represents its dynamic environment. The `come-from` has as successors both the "normal" successor, i.e. the body of the `cl:block` or the prefix of the `cl:tagbody`, and any iblocks that can be exited to, i.e. the result of the `cl:block` or the end of the `cl:tagbody`.

`come-from` instructions are also, fairly uniquely, values. In this capacity they represent the [continuation](https://en.wikipedia.org/wiki/Continuation). The continuation does in general need to exist at runtime and be closed over, so that at runtime it is clear what continuation/stack frame is being returned to.

One of the transformation passes, `eliminate-come-froms`, will automatically detect and delete any `come-from` instructions not used for nonlocal exits. This is important because in practice, most exits are within one function, and do not require any runtime dynamic environment.

The representation of nonlocal exits has gone through several iterations, and may go through more. Common Lisp seems fairly unique among programming languages in having continuations that are only used from fixed points in the program, and this ground is not well-trodden. The idea is to simply indicate the information known statically, such as where exits take place and where they end up, as well as provide enough information to generate code that takes any necessary dynamic actions, such as recording a stack pointer as an exit mark.

# Analyses and transformations

BIR has several properties intended to allow analyses and optimizations to be expressed simply and efficiently. Analysis passes may look through the IR and store information elsewhere, or in the IR itself. Optimization passes may transform the IR to new more efficient forms. Cleavir's own optimization passes mostly live in Cleavir/BIR-transformations/.

BIR can be mapped over efficiently with the functions and macros in map.lisp. These operators generally work without consing and in forward flow order, using internally maintained sequential lists.

# Verifier

The BIR verifier function `verify`, defined in verify.lisp, checks that a given module maintains invariants necessary for BIR to be well-formed. Compiler writers should use `verify` after passses if they are unsure that the pass's transformation is valid. Note that the verifier does not prove any kind of "correctness" of the Lisp code the BIR represents, or that the BIR accurately represents any source code; a verification failure merely indicates a bug in Cleavir's BIR handling, or in a client's pass.

`verify` returns silently if no problems were encountered. If the BIR is invalid, however, it will print a disassembly of the module and a listings of what conditions it found were violated.

# Disassembler

The BIR disassembler prints a textual representation of BIR for use in debugging compilers. The main entry point is `cleavir-bir-disassembler:display`. This function can be used on multiple levels of BIR structure, from a module down to an instruction, for when only part of a module needs to be displayed. The disassembler output will look like that in the examples below.

The BIR visualizer system provides an alternate, more visual way to look at BIR.

# Examples

This section briefly describes how BIR represents various examples of Lisp code.

## Identity function

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

The function has no constants, only one function in its module, and only one iblock in that function. The iblock merely binds a `variable` datum X, immediately reads from it, and returns the result. The reason for the variable binding is that in BIR, function `argument`s are linear SSA data, i.e. may only be assigned once and used once, so in general an argument will need to be bound to an argument to allow Lisp code to work with it. In this case, however, there is only one binding and one read for the variable, so it could be deleted, and the `returni` can simply return the argument directly. This optimization can be performed by the "delete temporary variables" transform in BIR-transformations.

## Calls, constants

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
     (constant-fdefinition 'foo) -> 0
     (readvar x) -> X-0
     (call foo x-0) -> 1
     (readvar x) -> X-1
     (call foo x-1) -> 2
     (returni 2)
```

The function is unknown to the compiler, so it is looked up at runtime using the `constant-fdefinition` instruction.

Note that there are two `readvar` instructions. This is because the X variable is read more than once, and so can't be represented with a linear datum. A `readvar` instruction must exist for each different usage, even here where there is no actual need to perform any kind of new lookup operation at runtime. Similarly, a constant may be referenced any number of times, but there must be one `constant-fdefinition`, `constant-reference`, or `constant-symbol-value` instruction for each usage.

## Control flow

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

## Local functions and calls

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

## Nonlocal exit

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
     (come-from tag (block-nil block-nil-merge))
  iblock BLOCK-NIL ():
   dynenv = (COME-FROM IN (LAMBDA (F X))-START)
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

The outer function, `(lambda (f x))`, binds `x` and then uses the `come-from` instruction. This instruction indicates a nonlocal entrance point, i.e. a point another function can immediately exit to using `return-from`, `go`, etc. The outer function then proceeds to `BLOCK-NIL`, which creates a closure and calls `f` with this closure. If this call returns normally, the result is passed to `BLOCK-NIL-MERGE`.

The inner function simply reads the variable `x`, and then uses the `unwind` instruction. This performs a nonlocal exit. The second input to the `unwind` instruction, `tag`, represents the nonlocal entrance at the come-from, and may have to exist at runtime as a sort of closure variable representing the target stack frame. `unwind` passes its first input, the read variable, to `BLOCK-NIL-MERGE` in the outer function.

`BLOCK-NIL-MERGE` simply returns its input, whether it's received from the outer function, or a nonlocal exit from the inner function. The input `0` is a phi node that is only used once, but which can be assigned by multiple `jump` and `unwind` instructions.
