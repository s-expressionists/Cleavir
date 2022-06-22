(in-package #:cleavir-example)

(defun derive-return-type (identity argstype)
  (ctype.ext.tfun:derive-multiple-value-call (ctype.ext.tfun:find-tfun identity nil)
                                             argstype))
