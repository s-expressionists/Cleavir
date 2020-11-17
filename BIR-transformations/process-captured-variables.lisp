(in-package #:cleavir-bir-transformations)

;;; Add LEXICAL onto the environment of ACCESS-FUNCTION and do so
;;; recursively.
(defun close-over (function access-function lexical)
  (unless (eq function access-function)
    (let ((environment (cleavir-bir:environment access-function)))
      (unless (cleavir-set:presentp lexical environment)
        (cleavir-set:nadjoinf environment lexical)
        (cleavir-set:doset (enclose (cleavir-bir:encloses access-function))
          (close-over function (cleavir-bir:function enclose) lexical))
        (cleavir-set:doset (local-call (cleavir-bir:local-calls access-function))
          (close-over function (cleavir-bir:function local-call) lexical))))))

;;; Fill in the environments of every function.
(defun determine-function-environments (module)
  (cleavir-set:doset (function (cleavir-bir:functions module) (values))
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
;;; calls with the DX-call attribute. Make sure we only mark closures
;;; as dynamic extent and do not try to mark a function as indefinite
;;; extent, since there may be an explicit dynamic extent declaration
;;; on the function which we should preserve.
(defun determine-closure-extents (module)
  (flet ((safe-call (call)
           (cleavir-attributes:has-boolean-attribute-p
            (cleavir-bir:attributes call)
            :dx-call)))
    (cleavir-set:doset (function (cleavir-bir:functions module))
      (cleavir-set:doset (enclose (cleavir-bir:encloses function))
        (unless (cleavir-bir:unused-p enclose)
          (let ((use (cleavir-bir:use enclose)))
            (typecase use
              (cleavir-bir:call
               (when (safe-call use)
                 (setf (cleavir-bir:extent enclose) :dynamic)))
              (cleavir-bir:writevar
               (let ((variable (first (cleavir-bir:outputs use)))
                     (safe t))
                 (cleavir-set:doset (reader (cleavir-bir:readers variable))
                   (unless (cleavir-bir:unused-p reader)
                     (let ((use (cleavir-bir:use reader)))
                       (typecase use
                         (cleavir-bir:call
                          (setq safe (safe-call use)))
                         (t (setq safe nil))))))
                 (when safe
                   (setf (cleavir-bir:extent enclose) :dynamic)))))))))))

(defun function-extent (function)
  (cleavir-set:doset (enclose (cleavir-bir:encloses function))
    (when (eq (cleavir-bir:extent enclose) :indefinite)
      (return-from function-extent :indefinite)))
  :dynamic)

;;; Determine the extent of every variable in a function based on the
;;; extent of any functions which close over it.
;;; Precondition: environments of functions must be filled in, and it
;;; helps to also analyze closure extent beforehand.
(defun determine-variable-extents (module)
  ;; First, initialize the extent of every variable.
  (cleavir-set:doset (function (cleavir-bir:functions module))
    (cleavir-set:doset (variable (cleavir-bir:variables function))
      (setf (cleavir-bir:extent variable) :local)))
  ;; Fill in the extent of every closed over variable.
  (cleavir-set:doset (function (cleavir-bir:functions module))
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
