(in-package #:cleavir-abstract-interpreter)

(defclass slots (pessimism) ())

(defclass sequential-slots (slots sequential) ())

(defmethod info ((strategy slots) (domain asserted-type) (datum bir:datum))
  (bir:asserted-type datum))
(defmethod (setf info) (new (strategy slots)
                        (domain asserted-type) (datum bir:datum))
  (setf (bir:asserted-type datum) new))

(defmethod info ((strategy slots) (domain derived-type) (datum bir:datum))
  (bir:ctype datum))
(defmethod (setf info) (new (strategy slots)
                        (domain derived-type) (datum bir:datum))
  (setf (bir:derived-type datum) new))

(defmethod info ((strategy slots) (domain attribute) (datum bir:datum))
  (bir:attributes datum))
(defmethod (setf info) (new (strategy slots)
                        (domain attribute) (datum bir:datum))
  (setf (bir:attributes datum) new))
