(defpackage #:cleavir-lambda-list
  (:use #:cl)
  (:export #:parameter-groups)
  (:export #:required-parameter-group-p
           #:optional-parameter-group-p
           #:rest-parameter-group-p
           #:key-parameter-group-p
           #:standard-parameter-group-p
           #:key-parameter-group-allows-other-keys-p)
  (:export #:parameters)
  (:export #:simple-parameter-variable
           #:optional-parameter-variable
           #:optional-parameter-supplied
           #:key-parameter-key
           #:key-parameter-variable
           #:key-parameter-supplied)
  (:export #:required-parameters
           #:optional-parameters
           #:rest-parameter
           #:keys-p #:key-parameters #:allow-other-keys-p)
  (:export #:make-lambda-list
           #:make-required-parameter-group
           #:make-optional-parameter-group
           #:make-rest-parameter-group
           #:make-key-parameter-group
           #:make-simple-parameter
           #:make-optional-parameter
           #:make-key-parameter)
  (:export #:map-lambda-list
           #:map-parameter-group
           #:map-simple-parameter
           #:map-optional-parameter
           #:map-key-parameter))
