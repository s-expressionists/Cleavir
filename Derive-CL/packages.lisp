(defpackage #:cleavir-derive-cl
  (:use #:cl)
  (:local-nicknames (#:ctype #:cleavir-ctype)
                    (#:domain #:cleavir-domain))
  (:export #:deriver)
  (:export #:derive-type-predicate)
  (:export #:generalized-true #:generalized-boolean)
  (:export #:simple-arrays-actually-adjustable-p #:array-dimension-limit-value
           #:array-rank-limit-value)
  (:export #:maximum-list-length #:maximum-sequence-length))
