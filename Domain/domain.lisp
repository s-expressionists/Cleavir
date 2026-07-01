(in-package #:cleavir-domain)

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
;;;; * WIDEN: Widening operator. This operates on an info and the next iteration
;;;;   of that info, and returns a possibly wider (i.e. greater) info.
;;;;   This is used in non-Noetherian domains to ensure that interpretation can
;;;;   be iterated finitely many times to reach a fixpoint.

(defclass domain () ())

(defgeneric infimum (client domain))
(defgeneric supremum (client domain))
(defgeneric subinfop (client domain info1 info2))
(defgeneric join/2 (client domain info1 info2))
(defgeneric meet/2 (client domain info1 info2))
(defgeneric widen (client domain old-info new-info))

;;;

(defun join (client domain &rest infos)
  (cond ((null infos) (infimum client domain))
        ((null (rest infos)) (first infos))
        (t (reduce (lambda (i1 i2) (join/2 client domain i1 i2)) infos))))
(define-compiler-macro join (&whole form client domain &rest infos)
  (cond ((null infos) `(infimum ,client ,domain))
        ((and (consp infos)
              (consp (cdr infos))
              (null (cddr infos)))
         `(join/2 ,client ,domain ,(first infos) ,(second infos)))
        (t form)))

(defun meet (client domain &rest infos)
  (cond ((null infos) (supremum client domain))
        ((null (rest infos)) (first infos))
        (t (reduce (lambda (i1 i2) (meet/2 client domain i1 i2)) infos))))
(define-compiler-macro meet (&whole form client domain &rest infos)
  (cond ((null infos) `(supremum ,client ,domain))
        ((and (consp infos)
              (consp (cdr infos))
              (null (cddr infos)))
         `(meet/2 ,client ,domain ,(first infos) ,(second infos)))
        (t form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Domains that are Noetherian, i.e. meet an ascending chain condition, so it
;;; is not possible to end up in an infinite loop of expanding infos.
;;; Basically this means no widening is necessary.

(defclass noetherian-mixin (domain) ())
(defmethod widen (client (domain noetherian-mixin) old new)
  (declare (ignore client old))
  new)
