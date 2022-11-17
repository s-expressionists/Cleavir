The `cleavir-ctype` system defines an interface for Cleavir to interact with a client's type system. This is necessary because the standard hook for working with types, `cl:subtypep`, cannot be used to efficiently implement the kind of type operations Cleavir needs. Clients may have more efficient representations than type specifiers, and may have operations for manipulating these representations more generally and efficiently.

This is not an implementation of the type system in itself. Without client customization, `cl:subtypep` will be used.

`cleavir-ctype` consists of three rough subcomponents: type operations, type construction, and type readers. Type operations are operations on types, such as `subtypep` and `conjoin`. Type construction functions create some client representation of a type, and type readers get information from these client representations. Throughout Cleavir, various operators refer to "ctypes" rather than "types" to mean these representations.

# Type operations

Besides `subtypep`, there are `conjoin`, `disjoin`, `negate`, `subtract`, and `disjointp`. These essentially continue the CL standard's treatment of types as sets. `disjointp`, which determines if two ctypes contain any common elements, is especially important for optimization.

# Type construction

The system contains generic functions for the various kinds of types defined by the standard - array types, real ranges, etc. These functions generally take an object representing the client as a parameter, so that methods may be defined, and then take other ctypes as more parameters.

# Handling of `values`

The `cl:values` types present some issue. The standard allows single-value types (that is, most of them) in essentially any place that values types are possible. Ctype is more explicit about the distinction. There are `values-` versions of the type operations; these versions take values ctypes as arguments, and the non-`values-` versions do not. `coerce-to-values`, `single-value`, `primary`, and `nth-value` can be used to convert to and from values ctypes.
