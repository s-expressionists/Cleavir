(in-package #:cleavir-attributes)

(defgeneric identities (attributes-designator)
  (:documentation "A list of objects used as identifiers for this function. These are used by BIR-TRANSFORMATIONS for client-defined, function-specific transformations, constant folds, type derivations, etc. Their nature is not defined by Cleavir, except that they can be compared with EQUAL, and that they should be externalizable if ASTs are to be externalized.
One easy choice is to use function names.
This is a list to account for data flow. For example, if a datum can originate from two separate definitions with different identities, the lists of identities are merged to reach a combined empty list of identities.

See BIR-TRANSFORMATIONS:TRANSFORM-CALL
See BIR-TRANSFORMATIONS:FOLD-CALL
See BIR-TRANSFORMATIONS:DERIVE-RETURN-TYPE"))

(defclass attributes ()
  (;; Boolean flags; see flags.lisp
   (%flags :initarg :flags :initform 0 :reader flags :type (integer 0))
   ;; FIXME: Might need some more thought on this.
   (%identities :initarg :identities :initform nil :reader identities))
  (:documentation "An object representing non-type attributes of a value."))

;;; We need to be able to externalize attributes for clients that externalize
;;; them as part of inline definition ASTs.
(defmethod make-load-form ((object attributes) &optional env)
  (make-load-form-saving-slots object :environment env))

(cleavir-io:define-save-info attributes
    (:flags (flags attributes))
  (:identities (identities attributes)))

(deftype attributes-designator ()
  "A designator for an attributes object, usable where an attributes object would be.
NIL means no attributes.
T means all attributes. This will never be the case for any real value, but is useful as a theoretical dual to NIL. It is analogous to the CL:NIL type.

See ATTRIBUTES"
  '(or attributes null (eql t)))

(defmethod flags ((attr null)) 0)
(defmethod identities ((attr null)) nil)

(defun default-attributes ()
  "Return the attributes that are safe to assume of any value."
  nil)

(defmethod flags ((attr (eql t))) -1)
;;; Technically this should be all possible identities, but in practice we
;;; want to behave as if nothing special is happening.
(defmethod identities ((attr (eql t))) nil)

(defgeneric has-flag-p (attributes-designator flag-name)
  (:documentation "Return true iff the attributes have the given flag on."))

(defmethod has-flag-p ((attributes null) flag-name)
  (declare (ignore flag-name))
  nil)
(defmethod has-flag-p ((attributes (eql t)) flag-name)
  (declare (ignore flag-name))
  t)
(defmethod has-flag-p ((attributes attributes) flag-name)
  (%has-flag-p (flags attributes) flag-name))

(defgeneric sub-attributes-p (attributes-designator-1 attributes-designator-2)
  (:documentation "Return true iff attributes-designator-1 is less specific than attributes-designator-2."))

(defmethod sub-attributes-p ((attr1 null) (attr2 null)) t)
(defmethod sub-attributes-p ((attr1 null) (attr2 attributes)) t)
(defmethod sub-attributes-p ((attr1 attributes) (attr2 null)) nil)
(defmethod sub-attributes-p ((attr1 attributes) (attr2 attributes))
  (and (sub-flags-p (flags attr1) (flags attr2))
       (subsetp (identities attr1) (identities attr2) :test #'equal)))
(defmethod sub-attributes-p ((attr1 null) (attr2 (eql t))) t)
(defmethod sub-attributes-p ((attr1 attributes) (attr2 (eql t))) t)
(defmethod sub-attributes-p ((attr1 (eql t)) (attr2 null)) nil)
(defmethod sub-attributes-p ((attr1 (eql t)) (attr2 attributes)) nil)
(defmethod sub-attributes-p ((attr1 (eql t)) (attr2 (eql t))) t)

(defgeneric meet-attributes (attributes-designator-1 attributes-designator-2)
  (:documentation "Return attributes combining both inputs; the returned attributes only have a given quality if both of the inputs do.

See JOIN-ATTRIBUTES"))
(defgeneric join-attributes (attributes-designator-1 attributes-designator-2)
  (:documentation "Dual to MEET-ATTRIBUTES.

See MEET-ATTRIBUTES"))

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
