(defpackage #:cleavir-ctype
  (:use #:common-lisp)
  (:shadow #:subtypep
           #:upgraded-array-element-type
           #:upgraded-complex-part-type
           #:cons #:array #:complex #:member
           #:satisfies #:function #:values #:nth-value
           #:funcall #:apply)
  (:export #:subtypep
           #:upgraded-array-element-type
           #:upgraded-complex-part-type)
  (:export #:conjoin/2 #:disjoin/2 #:negate #:subtract
           #:top #:bottom #:top-p #:bottom-p
           #:conjoin #:disjoin #:disjointp)
  (:export #:cons #:array #:complex #:range
           #:member #:satisfies #:function #:values
           #:coerce-to-values)
  (:export #:values-required #:values-optional #:values-rest
           #:nth-value #:primary #:single-value)
  (:export #:function-required #:function-optional #:function-rest
           #:function-keysp #:function-keys #:function-allow-other-keys-p
           #:function-values)
  (:export #:function-top)
  (:export #:values-subtypep)
  (:export #:apply #:funcall))
