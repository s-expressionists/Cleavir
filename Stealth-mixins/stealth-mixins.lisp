(cl:in-package :cleavir-stealth-mixins)

(defclass victim-class (standard-class)
  (;; A list of classes (not names!)
   (%stealth-mixins :initarg :stealth-mixins :initform nil
                    :reader stealth-mixins)))

(defmethod closer-mop:validate-superclass ((class victim-class)
                                           (super standard-class))
  t)

;;; This is sort of like UNION, except we need to preserve the order of
;;; the original direct superclasses, so we can't use UNION directly.
;;; (We leave the order of stealth mixins in relation to each other
;;;  unspecified.)
(defun merge-mixins (mixins original-supers)
  (loop for mixin in mixins
        do (setf original-supers (adjoin mixin original-supers)))
  original-supers)

(defmethod reinitialize-instance ((instance victim-class)
                                  &rest initargs
                                  &key (stealth-mixins nil smp)
                                    (direct-superclasses nil dsp))
  ;; If new mixins are being put in, we use the argument, and otherwise
  ;; we used the stored mixins. This means that non-stealth-mixin-aware
  ;; redefinitions, e.g. of the original defclass, use the stealth
  ;; information properly.
  (apply #'call-next-method instance
         :direct-superclasses (merge-mixins
                               (if smp
                                   stealth-mixins
                                   (stealth-mixins instance))
                               (if dsp
                                   direct-superclasses
                                   (closer-mop:class-direct-superclasses
                                    instance)))
         initargs))

(defun resolve-class-designator (class-designator)
  (if (typep class-designator 'class)
      class-designator
      (find-class class-designator)))

(defun add-stealth-mixin (mixin victim-class-name)
  ;; Add the class to the mixins of the victim.
  ;; The victim class must have been defined already.
  (let ((victim (find-class victim-class-name))
        (mixin (resolve-class-designator mixin)))
    (closer-mop:ensure-class
     victim-class-name
     :stealth-mixins (adjoin mixin (stealth-mixins victim))
     ;; Necessary because E-C-U-C is specified to use STANDARD-CLASS
     ;; if no :metaclass is provided, rather than the existing
     ;; metaclass.
     :metaclass (class-of victim))))

(defmacro define-stealth-mixin (name super-classes victim-class-desig
		                slots &rest options)
  "Like DEFCLASS but adds the newly defined class to the super classes
of 'victim-class'."
  `(progn
     ;; First define the class we talk about
     (defclass ,name ,super-classes ,slots ,@options)
     ,@(loop for victim-class in (if (listp victim-class-desig)
                                     victim-class-desig
                                     (list victim-class-desig))
             collect `(add-stealth-mixin ',name ',victim-class))
    ',name))
