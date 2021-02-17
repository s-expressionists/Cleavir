(in-package #:cleavir-flow)

(defclass flow ()
  ((%traversal :reader traversal :accessor %traversal)))

(defun gen-reader (name table-reader initf-reader)
  `(defun ,name (node flow)
     (let ((table (,table-reader flow)))
       (multiple-value-bind (value presentp) (gethash node table)
         (if presentp
             value
             (setf (gethash node table)
                   (funcall (,initf-reader flow) node)))))))
(defun gen-writer (name table-reader)
  `(defun ,name (new node flow)
     (setf (gethash node (,table-reader flow)) new)))

(defstruct pslotd name table-reader readers writers initf-name initf-form)

(defun parse-slotd (slotd)
  (loop with name = (car slotd)
        with initfunction
        for (key value) on (cdr slotd) by #'cddr
        when (eq key :initform)
          do (setf initfunction
                   (if initfunction
                       (error "Cannot specify more than one initfunction/initform in slotd ~a" slotd)
                       `(lambda (node) (declare (ignore node)) ,value)))
        else if (eq key :initfunction)
               do (setf initfunction
                        (if initfunction
                            (error "Cannot specify more than one initfunction/initform in slotd ~a" slotd)
                            value))
        else if (eq key :reader)
               collect value into readers
        else if (eq key :writer)
               collect value into writers
        else if (eq key :accessor)
               collect value into readers
               and collect `(setf ,value) into writers
        finally (return
                  (make-pslotd :name name
                               :readers readers
                               :writers writers
                               :table-reader (gensym (symbol-name name))
                               :initf-name (gensym (symbol-name name))
                               :initf-form (or initfunction
                                               `(lambda (node)
                                                  (declare (ignore node))
                                                  nil))))))

(defmethod mark ((flow flow) node)
  (mark (traversal flow) node))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro DEFINE-FLOW.
;;;
;;; (define-flow name (&key class direction &allow-other-keys) (&rest slotds))
;;;
;;; Defines a new dataflow analysis called NAME.
;;;
;;; This defines a new class called NAME. Instances of this class have mappings
;;; from graph nodes to information about those nodes. The nature of the
;;; mappings is specified by the SLOTDS argument.
;;;
;;; SLOTDS have syntax similar to DEFCLASS:
;;; (name &key initform initfunction reader writer accessor)
;;; Only one of INITFORM and INITFUNCTION may be defined.
;;; Readers will be defined as functions that take a graph node and an instance
;;; of the flow class as arguments, and return the information for that slot,
;;; node, and flow. Writers write similarly.
;;; The INITFORM and INITFUNCTION can be used to define the initial information
;;; for each node. If INITFORM is supplied, it will be evaluated for each node
;;; whose information is read before it is written. If INITFUNCTION is supplied,
;;; it should be a function of one argument; to provide the initial information
;;; for a node, this function will be called with the node as an argument.
;;;
;;; The analysis can be carried out by calling the function also defined by
;;; this macro, also called NAME. This function will create a new object of the
;;; flow class, perform the analysis to fill out its mappings, and then return
;;; the object.
;;;
;;; Keywords can be passed to the NAME function; these are used to customize the
;;; traversal. :class is the name of the traversal class to use, and the other
;;; keywords will be passed to MAKE-INSTANCE to produce the traversal.
;;; Any keywords not passed to the NAME function will be filled out from the
;;; keywords in the definition of the flow. If there is no default class, the
;;; traversal class WORKLIST will be used.
;;;
;;; See cleavir-liveness or cleavir-reaching-definitions for examples of how
;;; to define dataflow analyses.
;;;

(defmacro define-flow (name (&rest default-traversal-keys
                             &key (class nil classp) direction
                             &allow-other-keys)
                       (&rest slotds))
  (declare (ignore direction))
  (let ((pslotds (mapcar #'parse-slotd slotds))
        (default-traversal-classname (if classp class 'worklist)))
    (loop while (getf default-traversal-keys :class)
          do (remf default-traversal-keys :class))
    `(progn
       (defclass ,name (flow)
         (,@(loop for pslotd in pslotds
                  for name = (pslotd-name pslotd)
                  for table-reader = (pslotd-table-reader pslotd)
                  collect `(,name :reader ,table-reader
                                  :initform (make-hash-table :test #'eq)))
          ,@(loop for pslotd in pslotds
                  for initf-name = (pslotd-initf-name pslotd)
                  for initf-form = (pslotd-initf-form pslotd)
                  collect `(,initf-name :initform ,initf-form
                                        :allocation :class
                                        :reader ,initf-name))))
       ,@(loop for pslotd in pslotds
               for table-reader = (pslotd-table-reader pslotd)
               for initf-reader = (pslotd-initf-name pslotd)
               nconc (loop for reader in (pslotd-readers pslotd)
                           collect (gen-reader
                                    reader table-reader initf-reader))
               nconc (loop for writer in (pslotd-writers pslotd)
                           collect (gen-writer writer table-reader)))
       (defun ,name (graph &rest traversal-keys
                     &key (class nil classp) direction &allow-other-keys)
         (declare (ignore direction))
         (let* ((flow (make-instance ',name))
                (traversal
                  (apply #'make-instance
                         (find-class (if classp
                                         class
                                         ',default-traversal-classname))
                         (append traversal-keys ',default-traversal-keys))))
           (setf (%flow traversal) flow)
           (setf (%traversal flow) traversal)
           (initialize traversal graph)
           (work traversal graph)
           flow)))))
