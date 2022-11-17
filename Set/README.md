This system defines a basic set data structure, which is useful for various purposes.
Sets are especially used in BIR. The sets here are just the usual mathematical sense
of a collection of different elements. The usual operations, such as `union` and
membership checks, are defined. Some have CL's "n" prefix to indicate that they may
be carried out destructively for efficiency.

The `set` class is a wrapper over one of two implementations, a hash and a list. The
hash is used by default; if you want to use lists (which are more reproducible, but do
not have constant-time set membership checks), change the features code at the top of
`set.lisp`.

In the future, it would probably be useful to establish a more compact set representation,
as a bitfield with accompanying universe. Then different sets from the same universe
could be more compactly and efficiently used. The trick would be keeping the universe
stable in the face of modifications to the BIR or whatever else.
