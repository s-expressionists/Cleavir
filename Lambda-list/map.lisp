(in-package #:cleavir-lambda-list)

(defmethod map-lambda-list (function lambda-list system)
  (make-lambda-list
   (map 'list (lambda (parameter-group)
                (map-parameter-group function parameter-group system))
        (parameter-groups lambda-list))
   system))

(defmethod map-parameter-group (function parameter-group system)
  (cond ((required-parameter-group-p parameter-group system)
         (make-required-parameter-group
          (map 'list (lambda (parameter)
                       (map-simple-parameter function parameter system))
               (parameters parameter-group))
          system))
        ((optional-parameter-group-p parameter-group system)
         (make-optional-parameter-group
          (map 'list (lambda (parameter)
                       (map-optional-parameter function parameter system))
               (parameters parameter-group))
          system))
        ((rest-parameter-group-p parameter-group system)
         (make-rest-parameter-group
          (map 'list (lambda (parameter)
                       (map-rest-parameter function parameter system))
               (parameters parameter-group))
          system))
        ((key-parameter-group-p parameter-group system)
         (make-key-parameter-group
          (map 'list (lambda (parameter)
                       (map-key-parameter function parameter system))
               (parameters parameter-group))
          system))
        (t (call-next-method))))

(defmethod map-simple-parameter (function parameter system)
  (make-simple-parameter
   (funcall function (simple-parameter-variable parameter system))
   system))
(defmethod map-optional-parameter (function parameter system)
  (make-optional-parameter
   (funcall function (optional-parameter-variable parameter system))
   (funcall function (optional-parameter-supplied parameter system))
   system))
(defmethod map-key-parameter (function parameter system)
  (make-key-parameter
   (key-parameter-key parameter)
   (funcall function (key-parameter-variable parameter system))
   (funcall function (key-parameter-supplied parameter system))
   system))

;;; A few methods for the default lambda list constructs; these are redundant
;;; but more specific than the above map-parameter-group method.
(defmethod map-parameter-group (function (pg required-parameter-group) system)
  (make-required-parameter-group
   (map 'list (lambda (parameter)
                (map-simple-parameter function parameter system))
        (parameters parameter-group))
   system))
(defmethod map-parameter-group (function (pg optional-parameter-group) system)
  (make-optional-parameter-group
   (map 'list (lambda (parameter)
                (map-optional-parameter function parameter system))
        (parameters parameter-group))
   system))
(defmethod map-parameter-group (function (pg rest-parameter-group) system)
  (make-rest-parameter-group
   (map-simple-parameter function (elt (parameters pg) 0) system)
   system))
(defmethod map-parameter-group (function (pg key-parameter-group) system)
  (make-key-parameter-group
   (map 'list (lambda (parameter)
                (map-key-parameter function parameter system))
        (parameters parameter-group))
   system))
