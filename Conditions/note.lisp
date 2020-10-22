(in-package #:cleavir-conditions)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Condition type PROGRAM-NOTE
;;;
;;; PROGRAM-NOTE can be used for information that a programmer may wish to be
;;; informed of but which does not warrant even a STYLE-WARNING. This could
;;; include optimization hints or notes that the compiler will ignore some
;;; declaration because it is not smart enough to use it, for example.
;;; Essentially, there is no problem with the code itself per se, but the
;;; compiler is not doing its best with it.

(defclass program-note (program-condition) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function NOTE
;;;
;;; Signal a compiler note. The MUFFLE-NOTE restart can be used to prevent any
;;; reporting from being done. Otherwise, the note will be displayed on
;;; *error-output*.
;;; We have no simple-program-note class for reasons of internal hygeine,
;;; so unlike cl:error and cl:warn,  we only accept condition type specifiers
;;; as DATUM.

(defun note (datum &rest arguments)
  (let ((note (apply #'make-condition datum arguments)))
    (restart-case (signal note)
      (muffle-note ()
        :report "Silence note."
        (return-from note nil)))
    ;;; TODO: Maybe display origin or provide a way for clients to do so
    ;;; in some customizable fashion?
    (format *error-output* "~&;;; Note: ~a~%" note)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function MUFFLE-NOTE
;;;
;;; Invoke the MUFFLE-NOTE restart. If there is no active MUFFLE-NOTE restart,
;;; signals a CONTROL-ERROR. In either case, does not return.

(defun muffle-note (&optional condition)
  (invoke-restart (find-restart 'muffle-note condition)))
