(in-package #:cleavir-example)

;;;; Demonstration of constant folding.

(defparameter *constant-fold* t)
;; FIXME(paul) Remove one of the *folds*
(defparameter *folds* (make-hash-table :test #'equal))

(defmethod bir-transformations:fold-call ((client example) fold call args)
  (declare (ignore call))
  ;; *FOLDS* is defined in environment.lisp.
  (let ((folder (gethash fold *folds*)))
    (if (and *constant-fold* folder)
        (multiple-value-call #'values
          t
          (handler-case (apply folder args)
            (serious-condition (c)
              (warn "Serious condition interrupted constant folding:~%~a" c)
              (return-from bir-transformations:fold-call nil))))
        nil)))

(macrolet ((deffold (name)
             `(setf (gethash ',name *folds*) #',name))
           (deffolds (&rest names)
             `(progn ,@(loop for name in names collect `(deffold ,name)))))
  ;; Data and control flow
  (deffolds functionp eq eql equal equalp identity complement
    values values-list)
  ;; Arithmetic
  (deffolds = /= < > <= >= max min minusp plusp zerop
    ;; Note we can fold functions that return multiple values no problem
    floor ffloor ceiling fceiling truncate ftruncate round fround
    sin cos tan asin acos atan sinh cosh tanh asinh acosh atanh
    * + - / 1+ 1- abs evenp oddp exp expt gcd lcm log mod rem signum
    sqrt isqrt numberp cis complex complexp conjugate phase realpart imagpart
    realp numerator denominator rational rationalize rationalp
    ash integer-length integerp parse-integer boole
    logand logandc1 logandc2 logeqv logior lognand lognor lognot
    logorc1 logorc2 logxor logbitp logcount logtest
    ;; byte specifiers may not be externalizable, so we don't fold BYTE.
    ;; this is an example of when a client might make a foldability decision.
    byte-size byte-position deposit-field dpb ldb ldb-test mask-field
    decode-float scale-float float-radix float-sign float-digits
    float-precision integer-decode-float float floatp
    arithmetic-error-operands arithmetic-error-operation))
