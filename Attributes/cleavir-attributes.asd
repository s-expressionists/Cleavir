(in-package #:asdf-user)

;;;; Conceptual summary:

;;;; Data, especially functions, have many properties not expressible
;;;; in the CL type system. For example:
;;;; 1) whether their arguments can escape
;;;; 2) whether they call their arguments
;;;; 3) whether it can be constant folded, and how to do so
;;;; 4) whether they escape from their defining context
;;;; 5) how to determine the type of the return values from
;;;;    specifically some given argument types
;;;; 6) how to rewrite a call to be more efficient given type, extent,
;;;;    or other information

;;;; This subsystem encapsulates this information in an
;;;; "attributes" object. These attributes can be stored in the
;;;; environment (so that e.g. a compiler knows that AREF has no
;;;; side effects) before making their way into ASTs and HIR
;;;; where they can be used to validate transformations.

;;;; Attributes have a few differences from types. For the most part
;;;; they are impossible to test at runtime. They can be propagated
;;;; like types, and sometimes inferred like types, but usually
;;;; a meet or join operation won't return information as interesting
;;;; as that you might get from a type.

;;;; For clients: You can use make-attributes to make attributes to
;;;; return from CLEAVIR-ENV:FUNCTION-INFO etc.

;;;; TODO: All of this stuff should be system-customizable.
;;;; And right now only boolean attributes are supported.
;;;; Per-argument attributes might be good, as would a constant-fold
;;;; attribute that includes the constant-fold function.

(defsystem :cleavir-attributes
  :depends-on (:cleavir-io)
  :components
  ((:file "packages")
   (:file "flags" :depends-on ("packages"))
   (:file "attributes" :depends-on ("flags" "packages"))))
