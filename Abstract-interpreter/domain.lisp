(in-package #:cleavir-abstract-interpreter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; An abstract DOMAIN is a kind of information about parts of a program. Each
;;;; domain consists of a possibly infinite lattice of "info" objects of
;;;; domain-specific nature. As a lattice, each domain must have methods for
;;;; the interface's generic functions:
;;;; * INFIMUM: Return the least element in the lattice. Initially, all parts
;;;;   of the program have the infimum as their info.
;;;; * SUBINFOP: Determine if an info is less than or equal to another info
;;;;   with respect to the lattice's partial order. This should return values
;;;;   in the same way as CL:SUBTYPEP, i.e. it can return T T meaning true,
;;;;   NIL T meaning false, or NIL NIL meaning undetermined.
;;;; * JOIN/2: The lattice join operation with two info operands. The variable
;;;;   arity JOIN is defined on top of this generic function.
;;;; * SUPREMUM: Return the largest element in the lattice. This is used as a
;;;;   default when the abstract interpreter does not know how to handle some
;;;;   instruction.
;;;; * MEET/2: The lattice meet operation with two info operands.
;;;; * WJOIN/2: Widening join. This is like JOIN/2, but works on a Noetherian
;;;;   sublattice, i.e. some finite number of WJOIN operations will eventually
;;;;   converge. This is necessary for abstract interpretation to terminate.

(defclass domain () ())

(defgeneric infimum (domain))
(defgeneric subinfop (domain info1 info2))
(defgeneric join/2 (domain info1 info2))
(defgeneric meet/2 (domain info1 info2))

(defgeneric supremum (domain))
(defgeneric wjoin/2 (domain info1 info2))

;;;

(defun join (domain &rest infos)
  (cond ((null infos) (infimum domain))
        ((null (rest infos)) (first infos))
        (t (reduce (lambda (i1 i2) (join/2 domain i1 i2)) infos))))
(define-compiler-macro join (&whole form domain &rest infos)
  (cond ((null infos) `(infimum ,domain))
        ((and (consp infos)
              (consp (cdr infos))
              (null (cddr infos)))
         `(join/2 ,domain ,(first infos) ,(second infos)))
        (t form)))

(defun meet (domain &rest infos)
  (cond ((null infos) (supremum domain))
        ((null (rest infos)) (first infos))
        (t (reduce (lambda (i1 i2) (meet/2 domain i1 i2)) infos))))
(define-compiler-macro meet (&whole form domain &rest infos)
  (cond ((null infos) `(supremum ,domain))
        ((and (consp infos)
              (consp (cdr infos))
              (null (cddr infos)))
         `(meet/2 ,domain ,(first infos) ,(second infos)))
        (t form)))

(defun wjoin (domain &rest infos)
  (cond ((null infos) (infimum domain))
        ((null (rest infos)) (first infos))
        (t (reduce (lambda (i1 i2) (wjoin/2 domain i1 i2)) infos))))
(define-compiler-macro wjoin (&whole form domain &rest infos)
  (cond ((null infos) `(infimum ,domain))
        ((and (consp infos)
              (consp (cdr infos))
              (null (cddr infos)))
         `(wjoin/2 ,domain ,(first infos) ,(second infos)))
        (t form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Domains that are transmitted through def-use chains. They are insensitive
;;; to control flow in the sense that any datum is associated with one info,
;;; regardless of control. This means some information is lost, but it's still
;;; sound, and can be propagated with greater space and time efficiency.

(defclass data (domain) ())

(defclass forward-data (data) ())
(defclass backward-data (data) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Domains that pay attention to Lisp's multiple value semantics. Conceptually,
;;; an INFO for such a domain is a description of multiple values, each of which
;;; has its own SV-INFO (single value info), a different kind of thing from the
;;; INFO. This describes, for example, types. An extension interface for values
;;; domains is defined in values.lisp.

(defclass values-domain (domain) ())

(defclass forward-values-data (forward-data values-domain) ())
(defclass backward-values-data (backward-data values-domain) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Domains that are transmitted through control. Each info is associated with
;;; an instruction.

(defclass control (domain) ())

(defclass forward-control (control) ())
