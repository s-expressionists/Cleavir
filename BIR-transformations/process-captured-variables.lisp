(in-package #:cleavir-bir-transformations)

;;; Add LEXICAL onto the environment of ACCESS-FUNCTION and do so
;;; recursively.
(defun close-over (function access-function lexical)
  (unless (eq function access-function)
    (let ((environment (cleavir-bir:environment access-function)))
      (unless (cleavir-set:presentp lexical environment)
        (cleavir-set:nadjoinf environment lexical)
        (let ((enclose (cleavir-bir:enclose access-function)))
          (when enclose
            (close-over function (cleavir-bir:function enclose) lexical)))
        (cleavir-set:doset (local-call (cleavir-bir:local-calls access-function))
          (close-over function (cleavir-bir:function local-call) lexical))))))

;;; Fill in the environments of every function.
(defun determine-function-environments (module)
  (cleavir-bir:do-functions (function module)
    (cleavir-set:doset (variable (cleavir-bir:variables function))
      (cleavir-set:doset (reader (cleavir-bir:readers variable))
        (close-over function (cleavir-bir:function reader) variable))
      (cleavir-set:doset (writer (cleavir-bir:writers variable))
        (close-over function (cleavir-bir:function writer) variable)))
    (cleavir-set:doset (catch (cleavir-bir:catches function))
      (cleavir-set:doset (unwind (cleavir-bir:unwinds catch))
        (close-over function (cleavir-bir:function unwind) catch)))))

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
  (cleavir-attributes:has-boolean-attribute-p
   (cleavir-bir:attributes call)
   :dx-call))

(defun determine-closure-extent (function)
  (let ((enclose (cleavir-bir:enclose function)))
    (when enclose
      (let* ((eout (cleavir-bir:output enclose))
             (use (cleavir-bir:use eout)))
        (typecase use
          (cleavir-bir:call
           (when (safe-call-p use)
             (setf (cleavir-bir:extent enclose) :dynamic)))
          (cleavir-bir:writevar
           (let ((variable (cleavir-bir:output use))
                 (efunc (cleavir-bir:function enclose)))
             (cleavir-set:doset (reader (cleavir-bir:readers variable))
               (let* ((rout (cleavir-bir:output reader))
                      (use (cleavir-bir:use rout)))
                 (typecase use
                   (null)
                   (cleavir-bir:call
                    (unless (and (eq (cleavir-bir:function use) efunc)
                                 (safe-call-p use))
                      (return-from determine-closure-extent)))
                   (t (return-from determine-closure-extent)))
                 (setf (cleavir-bir:extent enclose) :dynamic))))))))))

(defun determine-closure-extents (module)
  (cleavir-bir:map-functions #'determine-closure-extent module))

(defun function-extent (function)
  (let ((enclose (cleavir-bir:enclose function)))
    (if enclose
        (cleavir-bir:extent enclose)
        :dynamic)))

;;; Determine the extent of every variable in a function based on the
;;; extent of any functions which close over it.
;;; Precondition: environments of functions must be filled in, and it
;;; helps to also analyze closure extent beforehand.
(defun determine-variable-extents (module)
  ;; First, initialize the extent of every variable.
  (cleavir-bir:do-functions (function module)
    (cleavir-set:doset (variable (cleavir-bir:variables function))
      (setf (cleavir-bir:extent variable) :local)))
  ;; Fill in the extent of every closed over variable.
  (cleavir-bir:do-functions (function module)
    (ecase (function-extent function)
      (:dynamic
       (cleavir-set:doset (lexical (cleavir-bir:environment function))
         (when (and (typep lexical 'cleavir-bir:variable)
                    (eq (cleavir-bir:extent lexical) :local))
           (setf (cleavir-bir:extent lexical) :dynamic))))
      (:indefinite
       (cleavir-set:doset (lexical (cleavir-bir:environment function))
         (when (typep lexical 'cleavir-bir:variable)
           (setf (cleavir-bir:extent lexical) :indefinite)))))))
