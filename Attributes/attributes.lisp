(in-package #:cleavir-attributes)

(defclass attributes ()
  (;; Boolean flags; see flags.lisp
   (%flags :initarg :flags :initform 0 :reader flags :type (integer 0))
   ;; A sequence of functions called to transform call instructions.
   (%transforms :initarg :transforms :initform nil
                :reader transforms :type list)))

(defmethod make-load-form ((object attributes) &optional env)
  ;; FIXME: We don't save transforms, since they're functions. That may
  ;; have negative consequences for inline ASTs.
  ;; This also means we have to initialize to NIL ourselves.
  (multiple-value-bind (create init)
      (make-load-form-saving-slots object
                                   :slot-names '(%flags) :environment env)
    (values create
            `(progn ,init (setf (slot-value ,object '%transforms) nil)))))

(cleavir-io:define-save-info attributes
    (:flags (flags attributes)))

;;; NIL means no special attributes.
(deftype attributes-designator () '(or attributes null))

(defmethod flags ((attr null)) 0)
(defmethod transforms ((attr null)) nil)

(defun default-attributes () nil)

(defgeneric has-flag-p (attributes flag-name))

(defmethod has-flag-p ((attributes null) flag-name)
  (declare (ignore has-flag-p))
  nil)
(defmethod has-flag-p ((attributes attributes) flag-name)
  (%has-flag-p (flags attributes) flag-name))

;;; Is attributes-1 less specific than attributes-2?
(defgeneric sub-attributes-p (attributes-1 attributes-2))

(defmethod sub-attributes-p ((attr1 null) (attr2 attributes)) t)
(defmethod sub-attributes-p ((attr1 attributes) (attr2 null)) nil)
(defmethod sub-attributes-p ((attr1 attributes) (attr2 attributes))
  (and (sub-flags-p (flags attr1) (flags attr2))
       (subsetp (transforms attr1) (transforms attr2))))

;;; Return attributes combining both inputs; the returned attributes
;;; only have a given quality if both of the inputs do. Because attributes
;;; are of function parameters, they are contravariant, and so this can be
;;; used like CL:OR types.
(defgeneric meet-attributes (attributes-1 attributes-2))
;;; Dual of the above.
(defgeneric join-attributes (attributes-1 attributes-2))

(defmethod meet-attributes ((attr1 null) (attr2 attributes)) attr1)
(defmethod meet-attributes ((attr1 attributes) (attr2 null)) attr2)
(defmethod meet-attributes ((attr1 attributes) (attr2 attributes))
  ;; Try to avoid consing.
  (cond ((sub-attributes-p attr1 attr2) attr1)
        ((sub-attributes-p attr2 attr1) attr2)
        (t (make-instance 'attributes
             :flags (meet-flags (flags attr1) (flags attr2))
             :transforms (intersection (transforms attr1)
                                       (transforms attr2))))))

(defmethod join-attributes ((attr1 null) (attr2 attributes)) attr2)
(defmethod join-attributes ((attr1 attributes) (attr2 null)) attr1)
(defmethod join-attributes ((attr1 attributes) (attr2 attributes))
  (cond ((sub-attributes-p attr1 attr2) attr2)
        ((sub-attributes-p attr2 attr1) attr1)
        (t (make-instance 'attributes
             :flags (join-flags (flags attr1) (flags attr2))
             :transforms (union (transforms attr1) (transforms attr2))))))
