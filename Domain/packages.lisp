(defpackage #:cleavir-domain
  (:use #:cl)
  (:local-nicknames (#:ctype #:cleavir-ctype)
                    (#:attributes #:cleavir-attributes))
  (:shadow #:type)
  (:export #:domain
           #:infimum #:supremum #:subinfop #:join/2 #:meet/2
           #:meet #:join #:widen)
  (:export #:values-mixin
           #:sv-infimum #:sv-supremum #:sv-subinfop
           #:sv-join/2 #:sv-meet/2 #:sv-widen
           #:values-info #:values-required #:values-optional #:values-rest
           #:info-values-nth #:primary #:single-value)
  (:export #:noetherian-mixin #:noetherian-values-mixin)
  (:export #:product #:domains #:product-info #:project)
  (:export #:with-info #:with-info-type #:deriver-lambda)
  ;; particular domains
  (:export #:type #:bits-used #:equivalence))
