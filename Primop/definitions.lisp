(in-package #:cleavir-primop-info)

(macrolet ((defprimops (&rest specs)
             `(progn
                ,@(loop for spec in specs
                        collect `(defprimop ,@spec)))))
  (defprimops
      (cleavir-primop:car 1 :value :flushable)
      (cleavir-primop:cdr 1 :value :flushable)
    (cleavir-primop:rplaca 2 :effect)
    (cleavir-primop:rplacd 2 :effect)
    (symbol-value 1 :value :flushable)
    ((setf symbol-value) 2 :effect)
    (fdefinition 1 :value :flushable)

    (cleavir-primop:slot-read 2 :value :flushable)
    (cleavir-primop:slot-write 3 :effect)
    (cleavir-primop:funcallable-slot-read 2 :value :flushable)
    (cleavir-primop:funcallable-slot-write 3 :effect)

    (cleavir-primop:fixnum-less 2 2 :flushable)
    (cleavir-primop:fixnum-not-greater 2 2 :flushable)
    (cleavir-primop:fixnum-equal 2 2 :flushable)))
