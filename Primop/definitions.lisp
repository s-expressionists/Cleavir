(in-package #:cleavir-primop-info)

(macrolet ((defprimops (&rest specs)
             `(progn
                ,@(loop for spec in specs
                        collect `(defprimop ,@spec)))))
  (defprimops
      (cleavir-primop:car (:object) (:object))
      (cleavir-primop:cdr (:object) (:object))
    (cleavir-primop:rplaca (:object :object) ())
    (cleavir-primop:rplacd (:object :object) ())
    (symbol-value (:object) (:object))
    ((setf symbol-value) (:object :object) ())
    (fdefinition (:object) (:object))

    (cleavir-primop:slot-read (:object :object) (:object))
    (cleavir-primop:slot-write (:object :object :object) ())
    (cleavir-primop:funcallable-slot-read (:object :object) (:object))
    (cleavir-primop:funcallable-slot-write (:object :object :object) ())

    (cleavir-primop:fixnum-less (:object :object) 2)
    (cleavir-primop:fixnum-not-greater (:object :object) 2)
    (cleavir-primop:fixnum-equal (:object :object) 2)))
