(in-package #:cleavir-lambda-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader functions
;;;

(defgeneric parameter-groups (lambda-list system)
  (:argument-precedence-order system lambda-list))

(defgeneric required-parameter-group-p (parameter-group system)
  (:argument-precedence-order system parameter-group))
(defgeneric optional-parameter-group-p (parameter-group system)
  (:argument-precedence-order system parameter-group))
(defgeneric rest-parameter-group-p (parameter-group system)
  (:argument-precedence-order system parameter-group))
(defgeneric key-parameter-group-p (parameter-group system)
  (:argument-precedence-order system parameter-group))

(defgeneric standard-parameter-group-p (parameter-group system)
  (:argument-precedence-order system parameter-group))

(defgeneric key-parameter-group-allows-other-keys-p
    (key-parameter-group system)
  (:argument-precedence-order system key-parameter-group))

(defgeneric parameters (parameter-group system)
  (:argument-precedence-order system parameter-group))

(defgeneric simple-parameter-variable (simple-parameter system)
  (:argument-precedence-order system simple-parameter))

(defgeneric optional-parameter-variable (optional-parameter system)
  (:argument-precedence-order system optional-parameter))
(defgeneric optional-parameter-supplied (optional-parameter system)
  (:argument-precedence-order system optional-parameter))

(defgeneric key-parameter-key (key-parameter system)
  (:argument-precedence-order system key-parameter))
(defgeneric key-parameter-variable (key-parameter system)
  (:argument-precedence-order system key-parameter))
(defgeneric key-parameter-supplied (key-parameter system)
  (:argument-precedence-order system key-parameter))

(defgeneric required-parameters (lambda-list system)
  (:argument-precedence-order system lambda-list))
(defgeneric optional-parameters (lambda-list system)
  (:argument-precedence-order system lambda-list))
(defgeneric rest-parameter (lambda-list system)
  (:argument-precedence-order system lambda-list))
(defgeneric keys-p (lambda-list system)
  (:argument-precedence-order system lambda-list))
(defgeneric key-parameters (lambda-list system)
  (:argument-precedence-order system lambda-list))
(defgeneric allow-other-keys-p (lambda-list system)
  (:argument-precedence-order system lambda-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Creation functions
;;;

(defgeneric make-lambda-list (parameter-groups system)
  (:argument-precedence-order system parameter-groups))

(defgeneric make-required-parameter-group (parameters system)
  (:argument-precedence-order system parameters))
(defgeneric make-optional-parameter-group (parameters system)
  (:argument-precedence-order system parameters))
(defgeneric make-rest-parameter-group (parameter system)
  (:argument-precedence-order system parameter))
(defgeneric make-key-parameter-group (parameters allow-other-keys-p system)
  (:argument-precedence-order system parameters allow-other-keys-p))

(defgeneric make-simple-parameter (variable system)
  (:argument-precedence-order system variable))
(defgeneric make-optional-parameter (variable supplied system)
  (:argument-precedence-order system variable supplied))
(defgeneric make-key-parameter (key variable supplied system)
  (:argument-precedence-order system variable supplied key))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mapping functions
;;;

(defgeneric map-lambda-list (function lambda-list system)
  (:argument-precedence-order system lambda-list function))
(defgeneric map-parameter-group (function parameter-group system)
  (:argument-precedence-order system parameter-group function))
(defgeneric map-simple-parameter (function parameter system)
  (:argument-precedence-order system parameter function))
(defgeneric map-optional-parameter (function parameter system)
  (:argument-precedence-order system parameter function))
(defgeneric map-key-parameter (function parameter system)
  (:argument-precedence-order system parameter function))
