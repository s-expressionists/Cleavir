(in-package #:cleavir-example)

(defmethod abstract-interpreter:derive-return-type ((instruction bir:abstract-call)
                                                    identity argstype (system example))
  (ctype.ext.tfun:derive-multiple-value-call (ctype.ext.tfun:find-tfun identity nil)
                                             argstype))
