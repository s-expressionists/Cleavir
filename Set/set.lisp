(in-package #:cleavir-set)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (push #+(or):listset #-(or):hashset *features*))

;;; hash sets, with a simple wrapper so they print nicely
(defclass set ()
  (#+listset(%list :initarg :list :accessor %setlist)
   #+hashset(%hash :initarg :hash :reader %hash)))

;; internal to this file
#+hashset
(declaim (inline hashset))
#+hashset
(defun hashset () (make-hash-table :test #'eq))

(defun empty-set ()
  #+hashset
  (make-instance 'set :hash (hashset))
  #+listset
  (make-instance 'set :list nil))

(defun empty-set-p (set)
  #+hashset
  (zerop (hash-table-count (%hash set)))
  #+listset
  (null (%setlist set)))

(defun size (set)
  #+hashset
  (hash-table-count (%hash set))
  #+listset
  (length (%setlist set)))

(defun make-set (&rest elements)
  #+hashset
  (loop with r = (hashset)
        for e in elements
        do (setf (gethash e r) t)
        finally (return (make-instance 'set :hash r)))
  #+listset
  (make-instance 'set :list (delete-duplicates elements)))

(defun presentp (item set)
  #+hashset
  (values (gethash item (%hash set)))
  #+listset
  (member item (%setlist set)))

(defmacro doset ((var setform &optional result) &body body)
  #+hashset
  `(loop for ,var being the hash-keys of (%hash ,setform)
         do (tagbody ,@body)
         finally (return ,result))
  #+listset
  `(dolist (,var (%setlist ,setform) ,result) ,@body))

(defun mapset (result-type f set)
  (ecase result-type
    ((nil) (doset (item set nil) (funcall f item)))
    ((set)
     #+hashset
     (let ((new (hashset)))
       (doset (item set (make-instance 'set :hash new))
              (setf (gethash (funcall f item) new) t)))
     #+listset
     (make-instance 'set :list (mapcar f (%setlist set))))
    ((list)
     #+hashset
     (let (r) (doset (item set r) (push (funcall f item) r)))
     #+listset
     (mapcar f (%setlist set)))))

(defun set-to-list (set) (mapset 'list #'identity set))

(defun copy-set (set)
  #+hashset
  (let ((result (hashset)))
    (doset (e set (make-instance 'set :hash result))
      (setf (gethash e result) t)))
  #+listset
  (make-instance 'set :list (copy-list (%setlist set))))

;;; Get an arbitrary element of the set, or else no values.
(defun arb (set)
  (doset (i set (values)) (return-from arb i)))

(defun set<= (set1 set2)
  (doset (i set1 t)
    (unless (presentp i set2) (return nil))))

(defun set= (set1 set2)
  (and (= (size set1) (size set2))
       (doset (i set1 t)
         (unless (presentp i set2) (return nil)))))

(defun some (p set)
  (doset (i set nil)
    (when (funcall p i) (return t))))

(defun every (p set)
  (doset (i set t)
    (unless (funcall p i) (return nil))))

(defun notany (p set)
  (doset (i set t)
    (when (funcall p i) (return nil))))

(defun notevery (p set)
  (doset (i set nil)
    (unless (funcall p i) (return t))))

(defun nadjoin (item set)
  #+hashset
  (setf (gethash item (%hash set)) t)
  #+listset
  (pushnew item (%setlist set))
  set)

;;; define-modify-macro reorders arguments and i don't like it
(defmacro nadjoinf (set item &environment env)
  (multiple-value-bind (temps values stores write read)
      (get-setf-expansion set env)
    `(let* (,@(mapcar #'list temps values))
       (multiple-value-bind (,@stores) (nadjoin ,item ,read)
         ,write))))

;; Removes an item from the set, returns the new set. Can be (is) destructive.
;; could be set-delete, but n is regular, if arbitrary
(defun nremove (item set)
  #+hashset
  (remhash item (%hash set))
  #+listset
  (setf (%setlist set) (delete item (%setlist set) :count 1))
  set)

(defmacro nremovef (set item &environment env)
  (multiple-value-bind (temps values stores write read)
      (get-setf-expansion set env)
    `(let* (,@(mapcar #'list temps values))
       (multiple-value-bind (,@stores) (nremove ,item ,read)
         ,write))))

;; Return an empty set, possibly destroying an existing set to do it.
(defun nset-empty (set)
  #+hashset
  (clrhash (%hash set))
  #+listset
  (setf (%setlist set) nil)
  set)

;; Only the first argument is destroyed.
(defun nunion (s1 s2)
  (doset (i s2 s1) (nadjoinf s1 i)))

(defun union (s1 s2)
  (let ((result (empty-set)))
    (doset (i s1) (nadjoinf result i))
    (doset (i s2) (nadjoinf result i))
    result))

(defmacro nunionf (set other &environment env)
  (multiple-value-bind (temps values stores write read)
      (get-setf-expansion set env)
    `(let* (,@(mapcar #'list temps values))
       (multiple-value-bind (,@stores) (nunion ,read ,other)
         ,write))))

(defun filter (result-type f set)
  (ecase result-type
    (set
     (let ((result (empty-set)))
       (doset (e set result)
         (when (funcall f e) (nadjoinf result e)))))
    (list
     (let ((result nil))
       (doset (e set result)
         (when (funcall f e) (push e result)))))))

(defun difference (result-type minuend subtrahend)
  (ecase result-type
    (set
     (let ((result (empty-set)))
       (doset (e minuend result)
         (unless (presentp e subtrahend) (nadjoinf result e)))))
    (list
     (let ((result nil))
       (doset (e minuend result)
         (unless (presentp e subtrahend) (push e result)))))))

;;; Different name from "difference" to reflect the different lambda list.
;;; FIXME: That's kind of ugly.
(defun nsubtract (minuend subtrahend)
  (doset (s subtrahend) (nremovef minuend s)))

(defmacro nsubtractf (minuend subtrahend &environment env)
  (multiple-value-bind (temps values stores write read)
      (get-setf-expansion minuend env)
    `(let* (,@(mapcar #'list temps values))
       (multiple-value-bind (,@stores) (nsubtract ,read ,subtrahend)
         ,write))))

(defmethod print-object ((s set) stream)
  (print-unreadable-object (s stream :type t)
    (format stream "{~{~a~^ ~}}" (set-to-list s))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *features*
        (remove #+listset :listset #+hashset :hashset *features*)))
