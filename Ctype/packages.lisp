(defpackage #:cleavir-ctype
  (:use #:common-lisp)
  (:shadow #:subtypep
           #:upgraded-array-element-type
           #:upgraded-complex-part-type
           #:class #:cons #:array #:string
           #:consp #:arrayp #:array-element-type #:array-dimensions
           #:character #:base-char #:standard-char
           #:complex #:fixnum #:member #:complexp
           #:satisfies #:keyword #:function #:compiled-function
           #:values #:nth-value #:funcall #:apply)
  (:export #:subtypep
           #:upgraded-array-element-type
           #:upgraded-complex-part-type)
  (:export #:conjoin/2 #:disjoin/2 #:negate #:subtract
           #:top #:bottom #:top-p #:bottom-p
           #:values-top #:values-bottom
           #:conjoin #:disjoin #:disjointp)
  (:export #:values-conjoin/2 #:values-disjoin/2 #:values-disjointp
           #:values-conjoin #:values-disjoin
           #:values-append/2 #:values-append)
  (:export #:wdisjoin/2 #:wdisjoin #:values-wdisjoin/2 #:values-wdisjoin)
  (:export #:class #:cons #:array #:string
           #:consp #:cons-car #:cons-cdr
           #:arrayp #:array-element-type #:array-dimensions
           #:character #:base-char #:standard-char
           #:complex #:range #:fixnum #:complexp #:complex-part-type
           #:rangep #:range-kind #:range-high #:range-low
           #:member #:member-p #:member-members
           #:satisfies #:keyword
           #:function #:compiled-function #:values #:coerce-to-values)
  (:export #:values-required #:values-optional #:values-rest
           #:nth-value #:primary #:single-value)
  (:export #:function-required #:function-optional #:function-rest
           #:function-keysp #:function-keys #:function-allow-other-keys-p
           #:function-values)
  (:export #:function-top)
  (:export #:values-subtypep)
  (:export #:apply #:funcall))
