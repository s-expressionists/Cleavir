(cl:in-package #:cleavir-compilation-policy)

;;; Public interface. Given a client, return its available
;;; policy qualities. The returned value is a proper list of entries
;;; of the form (NAME TYPE DEFAULT), where NAME is the name of the
;;; quality, TYPE is the type of allowable values of the policy, and
;;; DEFAULT is the value used for a declaration like (OPTIMIZE NAME).
(defgeneric policy-qualities (client)
  (:method-combination append))

;;; Private. A list to return from POLICY-QUALITIES's default
;;; method: policies Cleavir itself defines. Defined by
;;; DEFINE-CLEAVIR-POLICY-QUALITY.
;;; If an implementation wants to define its own policy qualities,
;;; it should specialize POLICY-QUALITIES.
(defvar *cleavir-policy-qualities* nil)

;;; Default method. Return Cleavir's policy qualities.
(defmethod policy-qualities append (client)
  *cleavir-policy-qualities*)

;;; Define a Cleavir policy quality, respecting redefinition.
(defun make-cleavir-policy-quality (name type default)
  (unless (typep default type)
    ;; FIXME: could be an error, but this is just a sanity check
    ;; anyway
    (warn 'bad-optimize-value :specifier `(,name ,default)
                              :expected type))
  (let ((a (assoc name *cleavir-policy-qualities*)))
    (if (null a)
        (push (list name type default) *cleavir-policy-qualities*)
        (setf (rest a) (list type default)))
    name))

;;; Defines a Cleavir policy quality. This way the definition can
;;; go in the actual system (type inference defines relevant type
;;; inference policies, that sort of thing).
(defmacro define-cleavir-policy-quality (name type default)
  `(make-cleavir-policy-quality ',name ',type ',default))
