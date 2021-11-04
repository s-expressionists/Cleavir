(in-package #:cleavir-lambda-list)

(defclass lambda-list ()
  ((%parameter-groups :initarg :parameter-groups :reader %parameter-groups)))

(defclass parameter-group ()
  ((%parameters :initarg :parameters :reader %parameters)))

(defclass required-parameter-group (parameter-group) ())
(defclass optional-parameter-group (parameter-group) ())
(defclass rest-parameter-group (parameter-group) ())
(defclass key-parameter-group (parameter-group)
  ((%aok-p :initarg :aok-p :reader %aok-p)))

(defclass optional-parameter ()
  ((%variable :initarg :variable :reader %variable)
   (%supplied :initarg :supplied :reader %supplied)))

(defclass key-parameter ()
  ((%key :initarg :key :reader %key)
   (%variable :initarg :variable :reader %variable)
   (%supplied :initarg :supplied :reader %supplied)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader interface
;;;

(defmethod parameter-groups ((lambda-list lambda-list) (system t))
  (%parameter-groups lambda-list))

(defmethod required-parameter-group-p ((pg required-parameter-group) (system t))
  t)
(defmethod required-parameter-group-p ((pg parameter-group) (system t)) nil)
(defmethod optional-parameter-group-p ((pg optional-parameter-group) (system t))
  t)
(defmethod optional-parameter-group-p ((pg parameter-group) (system t)) nil)
(defmethod rest-parameter-group-p ((pg rest-parameter-group) (system t)) t)
(defmethod rest-parameter-group-p ((pg parameter-group) (system t)) nil)
(defmethod key-parameter-group-p ((pg key-parameter-group) (system t)) t)
(defmethod key-parameter-group-p ((pg parameter-group) (system t)) nil)

;;; This method is written in terms of the others, so that a client may not
;;; need to specialize it even if they do specialize the above.
(defmethod standard-parameter-group-p (parameter-group system)
  (or (required-parameter-group-p parameter-group system)
      (optional-parameter-group-p parameter-group system)
      (rest-parameter-group-p parameter-group system)
      (key-parameter-group-p parameter-group system)))

(defmethod key-parameter-group-allows-other-keys-p ((pg key-parameter-group)
                                                    (system t))
  (%aok-p pg))

(defmethod parameters ((pg parameter-group) (system t)) (%parameters pg))

(defmethod optional-parameter-variable ((op optional-parameter) (system t))
  (%variable op))
(defmethod optional-parameter-supplied ((op optional-parameter) (system t))
  (%supplied op))
(defmethod key-parameter-key ((op key-parameter) (system t)) (%key op))
(defmethod key-parameter-variable ((op key-parameter) (system t))
  (%variable op))
(defmethod key-parameter-supplied ((op key-parameter) (system t))
  (%supplied op))

;;; These methods are written in terms of the others, so that a client may not
;;; need to specialize them even if they do specialize the above.
(defmethod required-parameters (lambda-list system)
  (let ((req (find-if (lambda (pg) (required-parameter-group-p pg system))
                      (parameter-groups lambda-list system))))
    (if req
        (parameters req)
        nil)))
(defmethod optional-parameters (lambda-list system)
  (let ((opt (find-if (lambda (pg) (optional-parameter-group-p pg system))
                      (parameter-groups lambda-list system))))
    (if opt
        (parameters opt)
        nil)))
(defmethod rest-parameter (lambda-list system)
  (let ((rest (find-if (lambda (pg) (rest-parameter-group-p pg system))
                       (parameter-groups lambda-list system))))
    (if rest
        (elt (parameters rest) 0)
        nil)))
(defmethod keys-p (lambda-list system)
  ;; position-if to head off the unlikely but possible situation in which a
  ;; client defines NIL to be a key parameter group.
  (if (position-if (lambda (pg) (key-parameter-group-p pg system))
                   (parameter-groups lambda-list system))
      t
      nil))
(defmethod key-parameters (lambda-list system)
  (let ((key (find-if (lambda (pg) (key-parameter-group-p pg system))
                      (parameter-groups lambda-list system))))
    (if key
        (parameters key)
        nil)))
(defmethod allow-other-keys-p (lambda-list system)
  (let ((key (find-if (lambda (pg) (key-parameter-group-p pg system))
                      (parameter-groups lambda-list system))))
    (if key
        (key-parameter-group-allows-other-keys-p pg system)
        nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Creation interface
;;;

(defmethod make-lambda-list ((parameter-groups sequence) (system t))
  (make-instance 'lambda-list :parameter-groups parameter-groups))

(defmethod make-required-parameter-group ((parameters sequence) (system t))
  (make-instance 'required-parameter-group :parameters parameters))
(defmethod make-optional-parameter-group ((parameters sequence) (system t))
  (make-instance 'optional-parameter-group :parameters parameters))
(defmethod make-rest-parameter-group (parameter (system t))
  (make-instance 'rest-parameter-group :parameters (list parameter)))
(defmethod make-key-parameter-group ((parameters sequence) aok-p (system t))
  (make-instance 'key-parameter-group :parameters parameters :aok-p aok-p))

(defmethod make-optional-parameter (variable supplied (system t))
  (make-instance 'optional-parameter :variable variable :supplied supplied))
(defmethod make-key-parameter (key variable supplied (system t))
  (make-instance 'key-parameter :key key :variable variable :supplied supplied))
