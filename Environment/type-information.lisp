(cl:in-package #:cleavir-environment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function TYPE-EXPAND.
;;;
;;; Performs type macroexpansion (macros being defined by DEFTYPE)
;;; in the given environment. Only top-level, but should expand
;;; repeatedly. That is to say, this is macroexpand, not
;;; macroexpand-1 or macroexpand-all.

(defgeneric type-expand (environment type-specifier))

(defmethod type-expand ((env entry) type-specifier)
  (type-expand (next env) type-specifier))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function FIND-CLASS.
;;;
;;; As CL:FIND-CLASS, but a generic function with different
;;; parameters so as to allow client specialization.

(defgeneric find-class (client name environment &optional errorp)
  (:argument-precedence-order client environment name))

;;; Default method: Use CL:FIND-CLASS.
(defmethod find-class (client name environment &optional errorp)
  (declare (cl:ignore client))
  (cl:find-class name errorp environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions PARSE-TYPE-SPECIFIER, PARSE-EXPANDED-TYPE-SPECIFIER,
;;; and PARSE-COMPOUND-TYPE-SPECIFIER.
;;;
;;; These functions serve to convert a type specifier into some
;;; object - a "ctype" - that is independent of the environment,
;;; and which can be used with the CLEAVIR-CTYPE protocol.
;;; The default behavior of the generic functions is designed to
;;; facilitate client implementations of the protocol.
;;;
;;; Given a type specifier and environment, PARSE-TYPE-SPECIFIER
;;; expands the specifier using TYPE-EXPAND (above), and then
;;; calls PARSE-EXPANDED-TYPE-SPECIFIER.
;;;
;;; The default methods on PARSE-EXPANDED-TYPE-SPECIFIER are
;;; as follows:
;;; * If the specifier is a class, it is passed to CTYPE:CLASS.
;;; * If the specifier is a symbol, it is looked up in the
;;;   environment using FIND-CLASS (above). If a class exists,
;;;   it is passed to CTYPE:CLASS. Otherwise, if it is a
;;;   standard atomic type specifier, it is passed to the appropriate
;;;   protocol function.
;;; * If the specifier is a cons, PARSE-COMPOUND-TYPE-SPECIFIER
;;;   is called using the CAR and CDR, and its value is returned.
;;;
;;; PARSE-COMPOUND-TYPE-SPECIFIER handles all standard compound
;;; type specifiers by calling the appropriate constructors in the
;;; CLEAVIR-CTYPE package.
;;;
;;; A client may need to extend methods here to implement nonstandard,
;;; non macro type specifiers specific to that implementation. It
;;; should not need to do so if it just has its own representation
;;; of types; CLEAVIR-CTYPE is what needs to be specialized there.

(defun parse-type-specifier (client type-specifier environment)
  (parse-expanded-type-specifier
   client
   (type-expand environment type-specifier)
   environment))

(defgeneric parse-expanded-type-specifier
    (client type-specifier environment)
  (:argument-precedence-order client environment type-specifier))

(defmethod parse-expanded-type-specifier
    (client (type-specifier symbol) environment)
  (let ((class (find-class client type-specifier environment nil)))
    (if class
        (ctype:class client class)
        (call-next-method))))

;;: Internal helper.
(defun parse-array-type-specifier
    (client simplicity element-type dimensions environment)
  (ctype:array
   client
   (if (eql element-type '*)
       element-type
       (ctype:upgraded-array-element-type
        client
        (parse-type-specifier client element-type environment)))
   dimensions simplicity))

(defmethod parse-expanded-type-specifier
    (client (ts (eql 'cl:array)) environment)
  (declare (cl:ignore environment))
  (ctype:array client '* '* 'array))

(defmethod parse-expanded-type-specifier
    (client (ts (eql 'cl:atom)) environment)
  (declare (cl:ignore environment))
  (let ((top (ctype:top client)))
    (ctype:negate client (ctype:cons client top top))))

(defmethod parse-expanded-type-specifier
    (client (ts (eql 'cl:base-char)) environment)
  (declare (cl:ignore environment))
  (ctype:base-char client))

(defmethod parse-expanded-type-specifier (client (ts (eql 'cl:base-string)) env)
  ;; Could just parse '(vector base-char) here, but I think it's better to
  ;; base-case as much as possible so that infinite regress is impossible.
  (parse-array-type-specifier client 'array 'base-char '(*) env))

(defmethod parse-expanded-type-specifier (client (ts (eql 'cl:bignum)) env)
  (declare (cl:ignore env))
  (ctype:conjoin
   client
   (ctype:range client 'integer '* '*)
   (ctype:negate client (ctype:fixnum client))))

(defmethod parse-expanded-type-specifier (client (ts (eql 'cl:bit-vector)) env)
  (parse-array-type-specifier client 'array 'bit '(*) env))

(defmethod parse-expanded-type-specifier (client (ts (eql 'cl:boolean)) env)
  (declare (cl:ignore env))
  (ctype:member client nil t))

(defmethod parse-expanded-type-specifier (client (ts (eql 'cl:character)) env)
  (declare (cl:ignore env))
  (ctype:character client))

(defmethod parse-expanded-type-specifier
    (client (ts (eql 'cl:compiled-function)) env)
  (declare (cl:ignore env))
  (ctype:compiled-function client))

(defmethod parse-expanded-type-specifier (client (ts (eql 'cl:complex)) env)
  (declare (cl:ignore env))
  (ctype:complex client '*))

(defmethod parse-expanded-type-specifier (client (ts (eql 'cl:cons)) env)
  (declare (cl:ignore env))
  (let ((top (ctype:top client)))
    (ctype:cons client top top)))

(macrolet ((defreal (head)
             `(defmethod parse-expanded-type-specifier
                  (client (ts (eql ',head)) env)
                (declare (cl:ignore env))
                (ctype:range client ts '* '*)))
           (defreals (&rest heads)
             `(progn ,@(loop for head in heads collect `(defreal ,head)))))
  (defreals integer rational real float short-float single-float
    double-float long-float))

(defmethod parse-expanded-type-specifier (client (ts (eql 'cl:extended-char)) env)
  (declare (cl:ignore env))
  (ctype:conjoin client
                 (ctype:character client)
                 (ctype:negate client (ctype:base-char client))))

(defmethod parse-expanded-type-specifier (client (ts (eql 'cl:fixnum)) env)
  (declare (cl:ignore env))
  (ctype:fixnum client))

(defmethod parse-expanded-type-specifier
    (client (type-specifier (eql 'cl:function)) environment)
  (declare (cl:ignore environment))
  (ctype:function-top client))

(defmethod parse-expanded-type-specifier (client (ts (eql 'cl:keyword)) env)
  (declare (cl:ignore env))
  (ctype:keyword client))

(defmethod parse-expanded-type-specifier (client (ts (eql 'cl:nil)) env)
  (declare (cl:ignore env))
  (ctype:bottom client))

(defmethod parse-expanded-type-specifier (client (ts (eql 'cl:signed-byte)) env)
  (declare (cl:ignore env))
  (ctype:range client 'integer '* '*))

(defmethod parse-expanded-type-specifier (client (ts (eql 'cl:simple-array)) env)
  (declare (cl:ignore env))
  (ctype:array client '* '* 'simple-array))

(defmethod parse-expanded-type-specifier (client (ts (eql 'cl:simple-base-string))
                                          env)
  (parse-array-type-specifier client 'simple-array 'base-char '(*) env))

(defmethod parse-expanded-type-specifier (client (ts (eql 'cl:simple-bit-vector))
                                          env)
  (parse-array-type-specifier client 'simple-array 'bit '(*) env))

(defmethod parse-expanded-type-specifier (client (ts (eql 'cl:simple-string))
                                          env)
  (declare (cl:ignore env))
  (ctype:string client '* 'simple-array))

(defmethod parse-expanded-type-specifier (client (ts (eql 'cl:simple-vector))
                                          env)
  (parse-array-type-specifier client 'simple-array 't '(*) env))

(defmethod parse-expanded-type-specifier (client (ts (eql 'cl:standard-char))
                                          env)
  (declare (cl:ignore env))
  (ctype:standard-char client))

(defmethod parse-expanded-type-specifier (client (ts (eql 'cl:t)) env)
  (declare (cl:ignore env))
  (ctype:top client))

(defmethod parse-expanded-type-specifier (client (ts (eql 'cl:unsigned-byte)) env)
  (declare (cl:ignore env))
  (ctype:range client 'integer 0 '*))

(defmethod parse-expanded-type-specifier
    (client (type-specifier class) environment)
  (declare (cl:ignore environment))
  (ctype:class client type-specifier))

(defmethod parse-expanded-type-specifier
    (client (type-specifier cons) environment)
  (parse-compound-type-specifier
   client (first type-specifier) (rest type-specifier) environment))

(defgeneric parse-compound-type-specifier
    (client head rest environment)
  (:argument-precedence-order client environment head rest))

;;; Internal helper function
(defun parse-type-specifier-list (client list environment)
  (loop for spec in list
        collect (parse-type-specifier client spec environment)))

(defmethod parse-compound-type-specifier
    (client (head (eql 'and)) rest environment)
  (apply #'ctype:conjoin
         client
         (parse-type-specifier-list client rest environment)))

(defmethod parse-compound-type-specifier
    (client (head (eql 'array)) rest environment)
  (destructuring-bind (&optional (et '*) (dimensions '*)) rest
    (parse-array-type-specifier client head et dimensions environment)))

(defmethod parse-compound-type-specifier
    (client (head (eql 'base-string)) rest env)
  (destructuring-bind (&optional (dimension '*)) rest
    (parse-array-type-specifier client head 'base-char (list dimension) env)))

(defmethod parse-compound-type-specifier
    (client (head (eql 'bit-vector)) rest env)
  (destructuring-bind (&optional (dimension '*)) rest
    (parse-array-type-specifier client head 'bit (list dimension) env)))

(defmethod parse-compound-type-specifier
    (client (head (eql 'complex)) rest environment)
  (destructuring-bind (&optional (et '*)) rest
    (ctype:complex
     client
     (if (eql et '*)
         et
         (ctype:upgraded-complex-part-type
          client
          (parse-type-specifier client et environment))))))

(defmethod parse-compound-type-specifier
    (client (head (eql 'cons)) rest environment)
  (destructuring-bind (&optional (car 't) (cdr 't)) rest
    (when (eql car '*) (setf car 't))
    (when (eql cdr '*) (setf cdr 't))
    (ctype:cons
     client
     (parse-type-specifier client car environment)
     (parse-type-specifier client cdr environment))))

(defmethod parse-compound-type-specifier (client (head (eql 'eql)) rest env)
  (declare (cl:ignore env))
  (destructuring-bind (object) rest
    (ctype:member client object)))

(defun parse-function-type-lambda-list (client lambda-list env)
  ;; FIXME?: Use a parser generator.
  ;; NOTE: In the CLHS, part of the description doesn't mention
  ;; &allow-other-keys and other parts do. Given the actual
  ;; meaning of the specifier, describing calls, it seems
  ;; reasonable to allow &allow-other-keys after &key.
  (loop with state = nil
        with required with optional with rest with restp
        with keys with keyp with aokp
        for element in lambda-list
        do (case element
             ((&optional)
              ;; FIXME: Better errors for these
              (assert (member state '(nil)))
              (setf state element))
             ((&rest)
              (assert (member state '(nil &optional)))
              (setf state element restp t))
             ((&key)
              (assert (member state '(nil &optional after-&rest)))
              (setf state element keyp t))
             ((&allow-other-keys)
              (assert (member state '(&key)))
              (setf state element aokp t))
             (otherwise
              (ecase state
                ((nil)
                 (push (parse-type-specifier client element env)
                       required))
                ((&optional)
                 (push (parse-type-specifier client element env)
                       optional))
                ((&rest)
                 (setf rest (parse-type-specifier client element env)
                       state 'after-&rest))
                ((&key)
                 (destructuring-bind (key type) element
                   (push (list key
                               (parse-type-specifier client type env))
                         keys))))))
        finally
           ;; Make sure we don't have (... &rest)
           (assert (not (member state '(&rest))))
           (return (values (nreverse required)
                           (nreverse optional)
                           (if restp
                               rest
                               (ctype:bottom client))
                           keyp (nreverse keys) aokp))))

(defmethod parse-compound-type-specifier
    (client (head (eql 'cl:function)) rest environment)
  (destructuring-bind (&optional (arg '*) (value '*)) rest
    (multiple-value-call #'ctype:function
      client
      (if (eq arg '*)
          (values nil nil (ctype:top client) nil nil nil)
          (parse-function-type-lambda-list client arg environment))
      (if (eq value '*)
          (ctype:values client nil nil (ctype:top client))
          (parse-values-type-specifier client value environment)))))

(defmethod parse-compound-type-specifier
    (client (head (eql 'member)) rest environment)
  (declare (cl:ignore environment))
  (apply #'ctype:member client rest))

(defmethod parse-compound-type-specifier (client (head (eql 'mod)) rest env)
  (declare (cl:ignore env))
  (destructuring-bind (max) rest
    (ctype:range client 'integer 0 max)))

(defmethod parse-compound-type-specifier
    (client (head (eql 'not)) rest environment)
  (destructuring-bind (under) rest
    (ctype:negate
     client
     (parse-type-specifier client under environment))))

(defmethod parse-compound-type-specifier
    (client (head (eql 'or)) rest environment)
  (apply #'ctype:disjoin
         client
         (parse-type-specifier-list client rest environment)))

(defmethod parse-compound-type-specifier
    (client (head (eql 'satisfies)) rest environment)
  (declare (cl:ignore environment))
  (destructuring-bind (fname) rest
    (ctype:satisfies client fname)))

(defmethod parse-compound-type-specifier (client (head (eql 'signed-byte))
                                          rest env)
  (declare (cl:ignore env))
  (destructuring-bind (&optional (bits '*)) rest
    (if (eq bits '*)
        (ctype:range client 'integer '* '*)
        (let ((n (ash 1 (- bits 1))))
          (ctype:range client 'integer (- n) (- n 1))))))

(defmethod parse-compound-type-specifier
    (client (head (eql 'simple-array)) rest environment)
  (destructuring-bind (&optional (et '*) (dimensions '*)) rest
    (parse-array-type-specifier client head et dimensions environment)))

(defmethod parse-compound-type-specifier
    (client (head (eql 'simple-base-string)) rest env)
  (destructuring-bind (&optional (dimension '*)) rest
    (parse-array-type-specifier client head 'base-char (list dimension) env)))

(defmethod parse-compound-type-specifier
    (client (head (eql 'simple-string)) rest env)
  (declare (cl:ignore env))
  (destructuring-bind (&optional (dimension '*)) rest
    (ctype:string client dimension 'simple-array)))

(defmethod parse-compound-type-specifier (client (head (eql 'simple-vector))
                                          rest env)
  (destructuring-bind (&optional (dimension '*)) rest
    (parse-array-type-specifier client head 't (list dimension) env)))

(defmethod parse-compound-type-specifier (client (head (eql 'string)) rest env)
  (declare (cl:ignore env))
  (destructuring-bind (&optional (dimension '*)) rest
    (ctype:string client dimension 'array)))

(defmethod parse-compound-type-specifier (client (head (eql 'unsigned-byte))
                                          rest env)
  (declare (cl:ignore env))
  (destructuring-bind (&optional (bits '*)) rest
    (ctype:range client 'integer 0 (if (eq bits '*) '* (1- (ash 1 bits))))))

(defmethod parse-compound-type-specifier (client (head (eql 'vector)) rest env)
  (destructuring-bind (&optional (et '*) (dimension '*)) rest
    (parse-array-type-specifier client 'array et (list dimension) env)))

;;; Internal helper.
(defun parse-range (client head rest)
  (destructuring-bind (&optional (low '*) (high '*)) rest
    (ctype:range client head low high)))

(macrolet ((defreal (head)
             `(defmethod parse-compound-type-specifier
                  (client (head (eql ',head)) rest environment)
                (declare (cl:ignore environment))
                (parse-range client head rest)))
           (defreals (&rest heads)
             `(progn
                ,@(loop for head in heads
                        collect `(defreal ,head)))))
  (defreals integer rational real
    float short-float single-float double-float long-float))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function PARSE-VALUES-TYPE-SPECIFIER.
;;;
;;; Values types are not accepted by PARSE-TYPE-SPECIFIER due to
;;; their special semantics. This function handles them. If given
;;; a non-values type specifier, it will still return a values
;;; type specifier. The semantics of values ctypes are explained
;;; in the CLEAVIR-CTYPE package; this function properly applies
;;; the "fuzziness" required by THE, etc.
;;;
;;; A client with a different ctype implementation should
;;; specialize CTYPE:VALUES.

(defgeneric parse-compound-values-type-specifier (client head rest env))
(defgeneric parse-expanded-values-type-specifier (client spec env))

;;; Given lists of required and optional ctypes, and RESTP and the REST
;;; ctype (or some junk if RESTP is false), return
;;; (values required optional rest) with fuzziness applied. That is, the
;;; result is strict (at least (LENGTH REQUIRED) values must actually be
;;; supplied, etc.)
(defun fuzz-values-tspec (client required optional rest restp)
  ;; Quoth the standard:
  ;; "It is permissible for FORM to yield a different number of values than
  ;;  those that are specified by VALUE-TYPE, provided that the values for
  ;;  which types are declared are indeed of those types. Missing values are
  ;;  treated as NIL for the purposes of checking their types."
  ;; Based on this and the examples, I believe the consequences are as follows:
  ;; (1) if a &rest is not declared, &rest t is implicit. Not declaring &rest
  ;;     counts as not declaring those types, so they can be missing or w/e.
  ;; (2) if a suffix of the "required" types includes NULL, that value is not
  ;;     actually required, since missing values are treated as being NIL.
  ;; (3) if a required type is disjoint from NULL, it is actually required.
  ;; This is, frankly, a very messy part of the standard. The description in
  ;; THE completely contradicts the description of the VALUES specifier.
  (let* ((null (ctype:member client nil))
         (rest (if restp rest (ctype:top client)))
         ;; Find the actually-optional suffix of the required types.
         (rpos (position-if (lambda (ct) (ctype:disjointp client ct null))
                            required :from-end t))
         (rrpos (if rpos (1+ rpos) 0))
         (rreq (subseq required 0 rrpos))
         (opt (append (nthcdr rrpos required) optional)))
    (values rreq opt rest)))

(defun parse-values-type-specifier (client type-specifier environment)
  (let ((spec (type-expand environment type-specifier)))
    (if (consp spec)
        (parse-compound-values-type-specifier client (car spec) (cdr spec)
                                              environment)
        (parse-expanded-values-type-specifier client spec environment))))

;;; We assume all non-cons type specifiers are single value specifiers.
;;; So we treat this the same as if we'd seen (values whatever).
(defmethod parse-expanded-values-type-specifier (client spec env)
  (let ((sv (parse-expanded-type-specifier client spec env)))
    (multiple-value-bind (req opt rest)
        (fuzz-values-tspec client (list sv) nil nil nil)
      (ctype:values client req opt rest))))

(defmethod parse-compound-values-type-specifier (client (head (eql 'values))
                                                 rest env)
  (multiple-value-bind (req opt rest)
      (multiple-value-bind (req opt rest restp)
          (parse-values-type-lambda-list client rest env)
        (fuzz-values-tspec client req opt rest restp))
    (ctype:values client req opt rest)))

;;; Ditto parse-expanded-values-type-specifier above.
(defmethod parse-compound-values-type-specifier (client head rest env)
  (let ((sv (parse-compound-type-specifier client head rest env)))
    (multiple-value-bind (req opt rest)
        (fuzz-values-tspec client (list sv) nil nil nil)
      (ctype:values client req opt rest))))

(defun parse-values-type-lambda-list (client lambda-list env)
  (loop with state = nil
        with required with optional with rest with restp
        for element in lambda-list
        do (case element
             ((&optional)
              (assert (member state '(nil)))
              (setf state element))
             ((&rest)
              (assert (member state '(nil &optional)))
              (setf state element restp t))
             (t
              (let ((ctype (parse-type-specifier
                            client element env)))
                (ecase state
                  ((nil) (push ctype required))
                  ((&optional) (push ctype optional))
                  ((&rest) (setf rest ctype state 'after-&rest))))))
        finally
           (assert (not (member state '(&rest))))
           (return (values (nreverse required) (nreverse optional)
                           rest restp))))
