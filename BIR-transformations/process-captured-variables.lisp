(in-package #:cleavir-bir-transformations)

;;; Add LEXICAL onto the environment of ACCESS-FUNCTION and do so
;;; recursively.
(defun close-over (function access-function lexical)
  (unless (eq function access-function)
    (let ((environment (bir:environment access-function)))
      (unless (set:presentp lexical environment)
        (set:nadjoinf environment lexical)
        (let ((enclose (bir:enclose access-function)))
          (when enclose
            (close-over function (bir:function enclose) lexical)))
        (set:doset (local-call (bir:local-calls access-function))
          (close-over function (bir:function local-call) lexical))))))

;;; Fill in the environments of every function.
(defun determine-function-environments (module)
  (bir:do-functions (function module)
    (set:doset (variable (bir:variables function))
      (set:doset (reader (bir:readers variable))
        (close-over function (bir:function reader) variable))
      (set:doset (writer (bir:writers variable))
        (close-over function (bir:function writer) variable)))
    (set:doset (come-from (bir:come-froms function))
      (set:doset (unwind (bir:unwinds come-from))
        (close-over function (bir:function unwind) come-from)))))

;;; Determine the extent of closures. We mark closures created by
;;; ENCLOSE instructions as dynamic extent if all of its uses are
;;; calls with the DX-call attribute in the enclosing function.
;;; We only mark closures as dynamic extent and do not try to mark a
;;; function as indefinite extent, since there may be an explicit
;;; dynamic extent declaration on the function which we should preserve.

;;; This analysis is conservative and could be improved (FIXME).
;;; Functions that are closed over are never marked dynamic-extent, even
;;; in cases in which they could be, e.g. if they are only recursive.
(defun safe-call-p (call)
  (attributes:has-flag-p (bir:attributes call) :dx-call))

(defun determine-closure-extent (function)
  (let ((enclose (bir:enclose function)))
    (when enclose
      (let* ((eout (bir:output enclose))
             (use (bir:use eout)))
        (typecase use
          (bir:call (when (safe-call-p use)
                      (setf (bir:extent enclose) :dynamic)))
          (bir:writevar
           (let ((variable (bir:output use))
                 (efunc (bir:function enclose)))
             (set:doset (reader (bir:readers variable))
               (let* ((rout (bir:output reader))
                      (use (bir:use rout)))
                 (typecase use
                   (null)
                   (bir:call
                    (unless (and (eq (bir:function use) efunc)
                                 (safe-call-p use))
                      (return-from determine-closure-extent)))
                   (t (return-from determine-closure-extent)))
                 (setf (bir:extent enclose) :dynamic))))))))))

(defun determine-closure-extents (module)
  (bir:map-functions #'determine-closure-extent module))

(defun function-extent (function)
  (let ((enclose (bir:enclose function)))
    (if enclose
        (bir:extent enclose)
        :dynamic)))

;;; Determine the extent of every variable in a function based on the
;;; extent of any functions which close over it.
;;; Precondition: environments of functions must be filled in, and it
;;; helps to also analyze closure extent beforehand.
(defun determine-variable-extents (module)
  ;; First, initialize the extent of every variable.
  (bir:do-functions (function module)
    (set:doset (variable (bir:variables function))
      (setf (bir:extent variable) :local)))
  ;; Fill in the extent of every closed over variable.
  (bir:do-functions (function module)
    (ecase (function-extent function)
      (:dynamic
       (set:doset (lexical (bir:environment function))
         (when (and (typep lexical 'bir:variable)
                    (eq (bir:extent lexical) :local))
           (setf (bir:extent lexical) :dynamic))))
      (:indefinite
       (set:doset (lexical (bir:environment function))
         (when (typep lexical 'bir:variable)
           (setf (bir:extent lexical) :indefinite)))))))
