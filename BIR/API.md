Sets
====

Sets are an abstract set datatype defined by the `cleavir-set` subsystem. They are of class `set`.

At the moment sets only test elements by `eql`, i.e. an element is present in the set if it is `eql` to a member of the set.

Sets can be constructed with `empty-set`, or by `make-set` which puts its arguments in the new set. `copy-set` makes a new set with the same elements as an old set.

`nadjoin`, `nremove`, can be used to add and remove elements respectively, and `nunion` performs the union operation. `nadjoinf`, `nremovef`, and `nunionf` are modify macro versions of these.

`presentp` determines if an element is present in a set. `size` returns how many elements are in a set; `empty-set-p` can be used for the special case of determining whether there are no elements. `arb` returns an arbitrary element from th eset and `set=` determines if two sets have the same elements.

The operators `doset`, `mapset`, `set-to-list`, `filter`, and `every` can map behavior over sets. `doset` is like `dolist`. `mapset` is like `map`, except that rather than full types, only `nil`, `list`, and `cleavir-set:set` are the allowed return types. `filter` is like `remove-if-not`, but takes a return type argument as well, which works the same as with `mapset`. None of these operators guarantee any kind of order, but only hit each element in the set once. Adjoining or removing set elements while iterating over a set has undefined consequences.

Data
====

The top class is `datum`, which is abstract, i.e. it is not intended that direct instances are ever instantiated.

All data have a `name`, `rtype`, `definitions`, and `uses`, read (not, in general, written) with generic functions of the same names. The `name` is a symbol (`nil` indicating no name), while `definitions` and `uses` are sets of instructions. The first two properties are slots, but the other two can be stored in other ways, and any new subclass of `datum` itself will need to define them.

Definitions are uses are not directly writable by clients: graph modifications will update them appropriately when edits to the graph are made.

Under `datum` there is the abstract `linear-datum` class. `linear-datum` defines an additional slot and reader, `use`, which is the sole use of the datum. `uses` is specialized to make a set of this one use.

Also under `datum` is the abstract `ssa`, a datum with only one definition. The reader `definition` is provided and `definitions` will use it to make a set. However the `definition` is not a slot, as its nature can vary between `ssa` subclasses.

An `ssa` datum that is identified with its definition is a `value`. An `ssa` that is also a `linear-datum` is a `transfer`.

`constant`s and `argument`s are `transfer`s and `value`s.

`phi`s are linear data. They have an `iblock` they are an argument to. They have multiple definitions, which are computed from the iblock's predecessors and nonlocal entrances.

`variable`s are data. They have `writers` and `readers`, which are identified with their `definitions` and `uses` but are mutable. They have an `extent` indicating whether they are local to one function, or shared across multiple functions, and if they are shared, whether they only have dynamic extent. They have an `owner`, the function in which they are defined, and a `binder`, the `lexical-bind` (below) that defines them. They also maintain a set, the `encloses`, of `enclose` instructions that enclose over them.

Instructions
============

The abstract class of instructions is `instruction`. Instructions have `inputs` and `outputs`, which are both sequences of data. Both `inputs` and `outputs` are mutable by clients in general, but mutating outputs does not make sense for some instructions. Instructions also have a `predecessor` and `successor`, both of which are either an instruction or `nil`. Instructions also have an `iblock`. All and only instructions that begin iblocks have a `nil` predecessor; all and only instructions that terminate an iblock have a `nil` successor.

Instructions that always output one datum are identified with that datum and are `computation`s; `computation`s are `value`s and `transfer`s in addition to being `instruction`s. Other instructions are `operation`s.

Instructions that terminate a block are distinguished as `terminator`s; `terminators` have a `next` slot with a sequence of iblocks they can go to.

The dynamic environment and function of an instruction can be read with the `dynamic-environment` and `function` functions respectively. These indirect through the instruction's iblock.

Dynamic environments
====================

`dynamic-environment` is the abstract class of dynamic environments. Dynamic environments maintain their `scope`, the set of iblocks that have this dynamic environment. The function `parent` returns the parent dynamic environment of a dynamic environment, or `nil` if there is no parent (i.e. the dynamic environment is a function).

A dynamic environment that binds variables is an instance of the also-abstract class `lexical-bind`. The `bindings` of a `lexical-bind` can be read.

`function`s (below) are `lexical-bind`s, and some instructions (e.g. `catch`) are dynamic environments or `lexical-bind`s also.

Lexical variable binding is distinguished so that clients may easily see the dynamic extent of variable bindings for stack allocating closure cells.

Iblocks
=======

`iblocks` are straight line sequences of instructions. They have a `start` instruction and an `end` instruction, which may be the same; the `end` must specifically be a terminator. They have a `dynamic-environment` and a `function`. They have a sequence of `inputs`, all of which are `phi` nodes; usually this is an empty sequence.

Iblocks also maintain sets of `predecessors` and `entrances`, which are iblocks that do/don't (respectively) share the same function as the iblock, and which terminate into it.

Functions
=========

A `function` represents a target function. They have a set of `iblocks`, a `start` iblock, and an `end` iblock, except that the `end` may be `nil` if the function never returns. They have a `lambda-list`. They have a set of `variables` referenced within them, i.e. including both variables it owns and variables it closes over. They maintain the set of `encloses` over them.

Graph modifications
===================

Changes to control flow are generally done through this API, which maintains correctness of backpointers and other references.

`insert-instruction-before` and `insert-instruction-after` are identical to their HIR equivalents, i.e., given a new instruction and an existing instruction, they insert the new instruction before or after the existing instruction in its iblock. `insert-instruction-after` only makes sense if the existing instruction is not a terminator.

`delete-instruction` removes a non-terminator instruction from control flow and other references; it can only be used if all of its outputs are unused or have additional definitions, i.e. the system makes sure used outputs are defined.

`replace-uses` changes all uses of a datum to use another datum instead. `replace-computation` is a combination of `replace-uses` and `delete-instruction`, i.e., given a `computation`, it replaces all uses of it with some other datum, then deletes the computation. `delete-computation` deletes a computation directly, but requires that the computation is unused.

`replace-terminator` deletes a terminator instruction from its iblock and control flow in general, and replaces it with another terminator; it ensures predecessor sets are updated as it does.
