(in-package #:cleavir-abstract-interpreter)

(defclass slots (pessimism) ())

(defclass sequential-slots (slots sequential) ())

;;; Abstract interpreter fields are stored directly in data and/or instructions,
;;; via stealth mixins.
;;; FIXME: Maybe make this optionally loaded or something?

(cleavir-stealth-mixins:define-stealth-mixin datum () bir:datum
  ((%asserted-type :accessor asserted-type)
   (%derived-type :accessor derived-type)
   (%attributes :accessor attributes)))

(defmethod info ((strategy slots) (domain asserted-type) (datum datum))
  (asserted-type datum))
(defmethod (setf info) (new (strategy slots)
                        (domain asserted-type) (datum datum))
  (setf (asserted-type datum) new))

(defmethod info ((strategy slots) (domain derived-type) (datum datum))
  (derived-type datum))
(defmethod (setf info) (new (strategy slots)
                        (domain derived-type) (datum datum))
  (setf (derived-type datum) new))

(defmethod info ((strategy slots) (domain attribute) (datum datum))
  (attributes datum))
(defmethod (setf info) (new (strategy slots)
                        (domain attribute) (datum datum))
  (setf (attributes datum) new))

(cleavir-stealth-mixins:define-stealth-mixin instruction () bir:instruction
  ((%reachablep :accessor reachablep)))

(defmethod info ((strategy slots) (domain reachability) (inst instruction))
  (reachablep inst))
(defmethod (setf info) (new (strategy slots)
                        (domain reachability) (inst instruction))
  (setf (reachablep inst) new))
