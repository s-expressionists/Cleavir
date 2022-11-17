Data, especially functions, have many properties not expressible
in the CL type system. For example:

1. whether their arguments can escape
2. whether they call their arguments
3. whether it can be constant folded, and how to do so
4. whether they escape from their defining context
5. how to determine the type of the return values from
   specifically some given argument types
6. how to rewrite a call to be more efficient given type, extent,
   or other information

This subsystem encapsulates this information in an
"attributes" object. These attributes can be stored in the
environment (so that e.g. a compiler knows that AREF has no
side effects) before making their way into ASTs and IR
where they can be used to validate transformations.

Attributes have a few differences from types. For the most part
they are impossible to test at runtime. They can be propagated
like types, and sometimes inferred like types, but usually
a meet or join operation won't return information as interesting
as that you might get from a type.

For clients: You can use make-attributes to make attributes to
return from CLEAVIR-ENV:FUNCTION-INFO etc.

TODO: All of this stuff should be more client-customizable.
Per-argument attributes might be good.

# Flags

A flag is a binary on-off indicating some known information about a function.

Flags are generally organized so that their lack is the general case, i.e. if a flag is "positive" in that it enables transformations, it must be explicitly asserted. Another way of putting this is that a completely unknown function essentially has no flags.

## Currently available flags

`:NO-CALL` means that the function does not increase the number of ways its arguments can be called. That is, it does not call them itself, and does not enable calls to occur in new ways (e.g. by storing an argument in a global variable, where anybody could call it later). This weird phrasing is because function arguments could do these things themselves `(e.g. (labels ((foo (x) (push (cons #'foo x) *calls*))) ...))` and this does not implicate the NO-CALL-ness of any function that is passed them as an argument.
Implies DYN-CALL.

`:DYN-CALL` means that the function can only increase the number of ways its arguments can be called with ways that call the argument in a dynamic environment substantially identical to that of the `DYN-CALL` function. For example, `(lambda (f) (funcall f))` could be `DYN-CALL`, but `(lambda (f x) (let ((*x* x)) (funcall f)))` could not, as it calls its argument f in a different dynamic environment. This implies that arguments are dx-safe (TODO: attributes for that) because if `f` was e.g. stored in a global it could later be called in arbitrary dynamic environments.

`:DX-CALL` implies that the function's callable arguments do not escape. For example, the function `(lambda (f) (funcall f))` is `DX-CALL`, while `(lambda (f) f)` is not. FIXME: This is probably better expressed as a dynamic extent attribute on individual arguments.

`:FLUSHABLE` means the function does not side-effect, and that calls to it can be deleted if the value of the call is not used.
