(in-package #:cleavir-example)

;;;; This file defines the system, EXAMPLE, used for the example.
;;;; This is just an object used for discrimination, so it is
;;;; defined trivially.

(defclass example () ())

(defvar *system* (make-instance 'example)
  "An object representing the example system, for use specializing Cleavir generic functions.")
