(in-package #:cleavir-attributes)

;;;; Flags are generally organized so that their lack is the
;;;; general case, i.e. if a flag is "positive" in that it
;;;; enables transformations, it must be explicitly asserted.
;;;; Another way of putting this is that a completely unknown
;;;; function essentially has no flags.

;;;; Current flags:

;;; :NO-CALL means that the function does not increase the number of
;;; ways its arguments can be called. That is, it does not call
;;; them itself, and does not enable calls to occur in new ways
;;; (e.g. by storing an argument in a global variable, where anybody
;;;  could call it later). This weird phrasing is because function
;;; arguments could do these things themselves
;;; (e.g. (labels ((foo (x) (push (cons #'foo x) *calls*))) ...))
;;; and this does not implicate the NO-CALL-ness of any function
;;; that is passed them as an argument.
;;; Implies DYN-CALL.

;;; :DYN-CALL means that the function can only increase the number
;;; of ways its arguments can be called with ways that call the
;;; argument in a dynamic environment substantially identical to
;;; that of the DYN-CALL function.
;;; For example, (lambda (f) (funcall f)) could be DYN-CALL,
;;; but (lambda (f x) (let ((*x* x)) (funcall f))) could not, as
;;; it calls its argument f in a different dynamic environment.
;;; This implies that arguments are dx-safe (TODO: attributes for
;;; that) because if f was e.g. stored in a global it could later
;;; be called in arbitrary dynamic environments.

;;; :DX-CALL implies that the function's callable arguments do not
;;; escape. For example, the function (lambda (f) (funcall f)) is
;;; DX-CALL, while (lambda (f) f) is not. FIXME: This is probably
;;; better expressed as a dynamic extent attribute on individual
;;; arguments.

;;; :FLUSHABLE means the function does not side-effect and calls to it
;;; can be deleted if the value of the call is not used.

;;; We represent boolean flags as an integer bitfield.

(defun make-flags (&rest flags)
  (let ((result 0))
    (dolist (flag flags)
      (let ((bits (ecase flag
                    ((:no-call) #b11)
                    ((:dyn-call) #b10)
                    ((:dx-call) #b100)
                    ((:flushable) #b1000))))
        (setf result (logior result bits))))
    result))

(defun sub-flags-p (flags1 flags2) (zerop (logandc2 flags1 flags2)))

(defun meet-flags (flags1 flags2) (logand flags1 flags2))
(defun join-flags (flags1 flags2) (logior flags1 flags2))

(defun %has-flag-p (flags flag-name)
  (logbitp
   (ecase flag-name
     ((:no-call) 0)
     ((:dyn-call) 1)
     ((:dx-call) 2)
     ((:flushable) 3))
   flags))
