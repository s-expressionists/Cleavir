(in-package #:cleavir-abstract-interpreter)

(defgeneric info (domain object))
(defgeneric (setf info) (new-info domain object)
  (:argument-precedence-order domain object new-info))
