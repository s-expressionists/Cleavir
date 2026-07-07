(in-package #:cleavir-derive-cl)

(defun and-bits (client type)
  (multiple-value-bind (low high emptyp) (type-integer-bounds client type)
    (if emptyp
        0 ; not an integer, so the logand is an error, so no bits are used
        (cond ((or (not low) (not high)) -1)
              ;; TODO: We could do better here, obviously.
              ;; e.g. for [4, 5], obviously only 5's bits are used.
              ;; But I'm not deep enough into Hacker's Delight right now,
              ;; and anyway, the use is probably marginal.
              (t (ldb (byte (max (integer-length low) (integer-length high)) 0)
                      -1))))))

(define-deriver (logand domain:bits-used)
    (client (&rest ints) domain:bits-used (used &rest ignore))
  (declare (ignore ignore))
  ;; Grab any bit ranges we can for the arguments, and use that to restrict
  ;; the result's used bits.
  (loop for int in (rest-infos client domain:bits-used ints)
        do (setf used (logand used (and-bits client int)))
        finally (domain:values-info client domain:bits-used () () used)))
