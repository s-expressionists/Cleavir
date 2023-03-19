Lisp has `OPTIMIZE` declarations for expressing that some
lexical block should be compiled with an emphasis on "speed",
and stuff like that. That's nice, but in the compiler's view,
vague. This system converts `OPTIMIZE` information into specific
and actionable "policies" for the rest of the compiler.

Policies are separate objects computed from the `OPTIMIZE` info.
A policy consists of several "qualities", which are just a
symbol naming it and a value. Qualities are computed from the
generic function `COMPUTE-POLICY-QUALITY`, which is called from
the overall `COMPUTE-POLICY` function.

What policies exist is defined by the `POLICY-QUALITIES`
generic function. Cleavir defines several of its own policies
with `DEFINE-CLEAVIR-COMPILER-POLICY`, but implementations can
as well for their own compiler transforms. `POLICY-QUALITIES`'s
return value has the same format as `OPTIMIZE-QUALITIES`, and an
`APPEND` method combo.

Every AST and instruction stores the policy that was computed
for its lexical region. (This means that policies should be
kept cached.) When a compiler transform wants to know whether
it should do something to such an object, it checks the
`POLICY-VALUE` for whatever it's doing.

For implementors:

1. If you have your own policies, return something from
   `POLICY-QUALITIES` when specified on your client.
   If you don't have your own policies don't sweat it.
2. Define a method for `COMPUTE-POLICY-QUALITY` specialized on
   your client and each policy quality (including Cleavir's).
   Example:

```lisp
(defmethod cleavir-policy:compute-policy-quality
    ((client my-client)
     (name (eql 'cleavir-typed-transforms:insert-type-checks))
     optimize)
  (= (cleavir-policy:optimize-value optimize 'safety) 3))
```

3. Make sure your client respects the `OPTIMIZE-INFO` protocol
   in `cleavir-environment`. You can use `COMPUTE-POLICY` to
   compute policies from optimize declarations, but you should
   avoid doing this on every `OPTIMIZE-INFO` call.
