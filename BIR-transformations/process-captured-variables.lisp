(in-package #:cleavir-bir-transformations)

(defun closed-over-predicate (function)
  (lambda (variable)
    (and (cleavir-bir:closed-over-p variable)
         (not (eq (cleavir-bir:function variable) function)))))

(defun mark-enclose-recursively (variables enclose)
  (let* ((owner (cleavir-bir:function enclose))
         (parents (cleavir-bir:encloses owner))
         (nparents (cleavir-set:size parents)))
    ;; mark the enclose and function
    (cleavir-set:nunionf (cleavir-bir:variables enclose) variables)
    (cleavir-set:nunionf (cleavir-bir:variables owner) variables)
    ;; Remove any variables the current function owns
    ;; and while we're at it, update the variables' enclose sets
    (cleavir-set:doset (v variables)
      (cleavir-set:nadjoinf (cleavir-bir:encloses v) enclose)
      (when (eq (cleavir-bir:function v) owner)
        (cleavir-set:nremovef variables v)))
    (cond (;; no more variables: nothing left to do
           (cleavir-set:empty-set-p variables))
          ((zerop nparents)) ; at the top: nothing left to do
          ((= nparents 1) ; only one parent, so the set can be destroyed
           (cleavir-set:doset (p parents)
             (mark-enclose-recursively variables p)))
          (t ; have to copy the set. (NOTE: We could skip one copy.)
           (cleavir-set:doset (p parents)
             (mark-enclose-recursively
              (cleavir-set:copy-set variables) p))))))

;;; Augment each enclose instruction with the set of variables that need to be
;;; closed over. Augment each function's variable set with any variables that
;;; need to be added for the encloses.
(defun transmit-variables (all-functions)
  (cleavir-set:doset (funct all-functions (values))
    (let ((closed (cleavir-set:filter
                   'cleavir-set:set
                   (closed-over-predicate funct)
                   (cleavir-bir:variables funct)))
          (encloses (cleavir-bir:encloses funct)))
      (if (= (cleavir-set:size encloses) 1)
          ;; only one node, so we can destroy the set
          (cleavir-set:doset (enclose encloses)
            (mark-enclose-recursively closed enclose))
          (cleavir-set:doset (enclose encloses)
            (mark-enclose-recursively (cleavir-set:copy-set closed)
                                      enclose))))))

(defun process-captured-variables (ir)
  (let ((af (cleavir-bir:functions (cleavir-bir:module ir))))
    (transmit-variables af)))
