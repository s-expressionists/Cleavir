(in-package #:cleavir-derive-cl)

(define-deriver (coerce domain:type) (client (object type))
  (ctype:single-value
   (if (constant-type-p client type)
       (let* ((spec (constant-type-value client type))
              (top (ctype:top client))
              (list (ctype:disjoin client (ctype:member client nil)
                                   (ctype:cons top top client))))
         ;; the result-type is actually dealt with in the standard as a symbol
         ;; rather than a type specifier, except for sequences.
         (case spec
           ((character) (ctype:character client))
           ((float short-float single-float double-float long-float)
            (ctype:range spec '* '* client))
           ((function) (ctype:function-top client))
           ((complex) (type-number client)) ; FIXME: subtler than most
           (otherwise
            (multiple-value-bind (ctype validp) (ctype:approximate-parse client spec)
              (if validp
                  (cond ((ctype:subtypep object ctype client) object)
                        ;; a type error should be signaled if the type specifies a
                        ;; length and the object is not of that length, so in safe
                        ;; code the result will be of that length. In unsafe code,
                        ;; uh, I guess we have a problem? FIXME??
                        ((ctype:subtypep ctype
                                         (ctype:array '* '(*) 'array client) client)
                         ctype)
                        ((ctype:subtypep ctype list client) ctype)
                        ;; in the default case we unfortunately can't just return
                        ;; ctype, because of the COMPLEX weirdness. TODO do better
                        (t (ctype:top client)))
                  (ctype:top client))))))
       (ctype:top client))
   client))

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
