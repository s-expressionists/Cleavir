(in-package #:cleavir-primop-info)

(macrolet ((defprimops (&rest specs)
             `(progn
                ,@(loop for spec in specs
                        collect `(defprimop ,@spec)))))
  (defprimops
      (cleavir-primop:car 1 (:object))
      (cleavir-primop:cdr 1 (:object))
    (cleavir-primop:rplaca 2 ())
    (cleavir-primop:rplacd 2 ())
    (symbol-value 1 (:object))
    ((setf symbol-value) 2 ())
    (fdefinition 1 (:object))

    (cleavir-primop:slot-read 2 (:object))
    (cleavir-primop:slot-write 3 ())
    (cleavir-primop:funcallable-slot-read 2 (:object))
    (cleavir-primop:funcallable-slot-write 3 ())

    (cleavir-primop:fixnum-less 2 2)
    (cleavir-primop:fixnum-not-greater 2 2)
    (cleavir-primop:fixnum-equal 2 2)))
