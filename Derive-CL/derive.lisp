(in-package #:cleavir-derive-cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; This system defines information derivers (mostly type derivers) for standard
;;;; CL operators. A deriver is an abstraction of a function: as arguments they
;;;; take abstractions of data instead of actual data, e.g. the types of arguments
;;;; rather than actual arguments, and they compute an abstraction as well, e.g.
;;;; the types of the return values.
;;;; Each deriver takes information from various domains, generally starting
;;;; with the types of the arguments because they are so useful.
;;;;
;;;; These derivers are not used for anything by Cleavir directly. Clients can
;;;; use them in the abstract interpreter or meta-evaluate.
;;;;

(defvar *derivers* (make-hash-table))

(defun deriver (domain operator-name)
  (let ((table (gethash domain *derivers*)))
    (or (if table
            (gethash operator-name table)
            nil)
        (compute-deriver domain operator-name))))

(defun default-deriver (domain)
  (flet ((default-deriver (client product info)
           (declare (ignore product info))
           (domain:supremum client domain)))
    #'default-deriver))

;;; compute a deriver from the table. This only makes sense for products.
;;; it doesn't get put into the table because cache invalidation would be
;;; annoying to deal with. FIXME
(defun compute-deriver (domain operator-name)
  (if (typep domain 'domain:product)
      (let* ((domains (domain:domains domain))
             (derivers (loop for domain in domains
                             collect (deriver domain operator-name))))
        (lambda (client product info)
          (domain:product client domain
                          (loop for domain in domains
                                for deriver in derivers
                                collect (if deriver
                                            (funcall deriver client product info)
                                            (domain:supremum client domain))))))
      (default-deriver domain)))

(defun (setf deriver) (new domain operator-name)
  (let ((table (or (gethash domain *derivers*)
                   (setf (gethash domain *derivers*)
                         ;; equal for setf function names
                         (make-hash-table :test #'equal)))))
    (setf (gethash operator-name table) new)))

(defun function-block-name (operator)
  (etypecase operator
    (symbol operator)
    ((cons (eql setf) (cons symbol null)) (second operator))))

(defmacro define-deriver ((operator domain) (client type &rest specs) &body body)
  `(setf (deriver ,domain ',operator)
         (domain:deriver-lambda (,client ,(function-block-name operator)
                                         ,domain ,type ,@specs)
           ,@body)))
