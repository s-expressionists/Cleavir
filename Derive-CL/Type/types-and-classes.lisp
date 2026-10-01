(in-package #:cleavir-derive-cl)

(define-deriver (typep domain:type)
    (client (object type-specifier &optional environment))
  (declare (ignore environment)) ; not sure if this is ok
  (if (constant-type-p client type-specifier)
      (let ((spec (constant-type-value client type-specifier)))
        (multiple-value-bind (ctype validp)
            (ctype:approximate-parse client spec)
          (if validp
              (derive-type-predicate client 'typep object ctype)
              (ctype:single-value (generalized-boolean client 'typep) client))))
       (ctype:single-value (generalized-boolean client 'typep) client)))
