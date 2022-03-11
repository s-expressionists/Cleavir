(in-package #:cleavir-attributes)

(defclass attributes ()
  (;; Boolean flags; see flags.lisp
   (%flags :initarg :flags :initform 0 :reader flags :type (integer 0))
   ;; A list of objects used as identifiers for this function. These are
   ;; used by BIR-TRANSFORMATIONS for client-defined, function-specific
   ;; transformations, constant folds, type derivations, etc.
   ;; Their nature is not defined by Cleavir, except that they can be
   ;; compared with EQUAL, and that they should be externalizable if ASTs
   ;; are to be externalized.
   ;; One easy choice is to use function names.
   ;; This is a list to account for data flow. For example, if a datum
   ;; can originate from two separate definitions with different identities,
   ;; the lists of identities are merged to reach a combined empty list of
   ;; identities.
   ;; See BIR-TRANSFORMATIONS:TRANSFORM-CALL, FOLD-CALL, DERIVE-RETURN-TYPE.
   ;; FIXME: Might need some more thought on this.
   (%identities :initarg :identities :initform nil :reader identities)))

;;; We need to be able to externalize attributes for clients that externalize
;;; them as part of inline definition ASTs.
(defmethod make-load-form ((object attributes) &optional env)
  (make-load-form-saving-slots object :environment env))

(cleavir-io:define-save-info attributes
    (:flags (flags attributes))
  (:identities (identities attributes)))

;;; NIL means no special attributes.
;;; T means all attributes. This will never be the case for any real function,
;;; but is useful as a theoretical dual to NIL. It's kind of like NIL type.
(deftype attributes-designator () '(or attributes null (eql t)))

(defmethod flags ((attr null)) 0)
(defmethod identities ((attr null)) nil)

(defun default-attributes () nil)

(defmethod flags ((attr (eql t))) -1)
;;; Technically this should be all possible identities, but in practice we
;;; want to behave as if nothing special is happening.
(defmethod identities ((attr (eql t))) nil)

(defgeneric has-flag-p (attributes flag-name))

(defmethod has-flag-p ((attributes null) flag-name)
  (declare (ignore flag-name))
  nil)
(defmethod has-flag-p ((attributes (eql t)) flag-name)
  (declare (ignore flag-name))
  t)
(defmethod has-flag-p ((attributes attributes) flag-name)
  (%has-flag-p (flags attributes) flag-name))

;;; Is attributes-1 less specific than attributes-2?
(defgeneric sub-attributes-p (attributes-1 attributes-2))

(defmethod sub-attributes-p ((attr1 null) (attr2 null)) t)
(defmethod sub-attributes-p ((attr1 null) (attr2 attributes)) t)
(defmethod sub-attributes-p ((attr1 attributes) (attr2 null)) nil)
(defmethod sub-attributes-p ((attr1 attributes) (attr2 attributes))
  (and (sub-flags-p (flags attr1) (flags attr2))
       (subsetp (identities attr1) (identities attr2) :test #'equal)))
(defmethod sub-attributes-p ((attr1 null) (attr2 (eql t))) t)
(defmethod sub-attributes-p ((attr1 attributes) (attr2 (eql t))) t)
(defmethod sub-attributes-p ((attr1 (eql t)) (attr2 (eql t))) t)

;;; Return attributes combining both inputs; the returned attributes
;;; only have a given quality if both of the inputs do. Because attributes
;;; are of function parameters, they are contravariant, and so this can be
;;; used like CL:OR types.
(defgeneric meet-attributes (attributes-1 attributes-2))
;;; Dual of the above.
(defgeneric join-attributes (attributes-1 attributes-2))

(defmethod meet-attributes ((attr1 null) (attr2 null)) attr1)
(defmethod meet-attributes ((attr1 null) (attr2 attributes)) attr1)
(defmethod meet-attributes ((attr1 attributes) (attr2 null)) attr2)
(defmethod meet-attributes ((attr1 attributes) (attr2 attributes))
  ;; Try to avoid consing.
  (cond ((sub-attributes-p attr1 attr2) attr1)
        ((sub-attributes-p attr2 attr1) attr2)
        (t (make-instance 'attributes
             :flags (meet-flags (flags attr1) (flags attr2))
             :identities (intersection (identities attr1)
                                       (identities attr2) :test #'equal)))))
(defmethod meet-attributes ((attr1 null) (attr2 (eql t))) attr1)
(defmethod meet-attributes ((attr1 (eql t)) (attr2 null)) attr2)
(defmethod meet-attributes ((attr1 attributes) (attr2 (eql t))) attr1)
(defmethod meet-attributes ((attr1 (eql t)) (attr2 attributes)) attr2)
(defmethod meet-attributes ((attr1 (eql t)) (attr2 (eql t))) attr1)

(defmethod join-attributes ((attr1 null) (attr2 null)) attr1)
(defmethod join-attributes ((attr1 null) (attr2 attributes)) attr2)
(defmethod join-attributes ((attr1 attributes) (attr2 null)) attr1)
(defmethod join-attributes ((attr1 attributes) (attr2 attributes))
  (cond ((sub-attributes-p attr1 attr2) attr2)
        ((sub-attributes-p attr2 attr1) attr1)
        (t (make-instance 'attributes
             :flags (join-flags (flags attr1) (flags attr2))
             :identities (union (identities attr1) (identities attr2)
                                :test #'equal)))))
(defmethod join-attributes ((attr1 null) (attr2 (eql t))) attr2)
(defmethod join-attributes ((attr1 (eql t)) (attr2 null)) attr1)
(defmethod join-attributes ((attr1 attributes) (attr2 (eql t))) attr2)
(defmethod join-attributes ((attr1 (eql t)) (attr2 attributes)) attr1)
(defmethod join-attributes ((attr1 (eql t)) (attr2 (eql t))) attr1)
