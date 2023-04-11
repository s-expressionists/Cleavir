(in-package #:cleavir-example)

;;;; This file defines the client, EXAMPLE, used for the example.
;;;; This is just an object used for discrimination, so it is
;;;; defined trivially.

(defclass example (trucler-reference:client) ())

(defvar *client* (make-instance 'example)
  "An object representing the example client, for use specializing Cleavir generic functions.")
