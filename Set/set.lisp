(in-package #:cleavir-set)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (push #+(or):listset #-(or):hashset *features*))

(defclass set ()
  (#+listset(%list :initarg :list :accessor %setlist)
   #+hashset(%hash :initarg :hash :reader %hash))
  (:documentation "A collection of different things."))

;; internal to this file
#+hashset
(declaim (inline hashset))
#+hashset
(defun hashset () (make-hash-table :test #'eq))

(defun empty-set ()
  "Return a fresh set containing no elements."
  #+hashset
  (make-instance 'set :hash (hashset))
  #+listset
  (make-instance 'set :list nil))

(defun empty-set-p (set)
  "Return true iff the given set has no elements."
  #+hashset
  (zerop (hash-table-count (%hash set)))
  #+listset
  (null (%setlist set)))

(defun size (set)
  "Return the number of elements in the set."
  #+hashset
  (hash-table-count (%hash set))
  #+listset
  (length (%setlist set)))

(defun make-set (&rest elements)
  "Return a fresh set containing the given elements, and only those elements."
  #+hashset
  (loop with r = (hashset)
        for e in elements
        do (setf (gethash e r) t)
        finally (return (make-instance 'set :hash r)))
  #+listset
  (make-instance 'set :list (delete-duplicates elements)))

(defun presentp (item set)
  "Check if ITEM is present in SET."
  #+hashset
  (values (gethash item (%hash set)))
  #+listset
  (member item (%setlist set)))

(defmacro doset ((var setform &optional result) &body body)
  "Map over the elements of SET. Analogous to DOLIST.
The order elements are mapped in is undefined.

See MAPSET"
  #+hashset
  `(loop for ,var being the hash-keys of (%hash ,setform)
         do (tagbody ,@body)
         finally (return ,result))
  #+listset
  `(dolist (,var (%setlist ,setform) ,result) ,@body))

(defun mapset (result-type f set)
  "Map over the elements of SET. Analogous to MAP.
Result type can be:
NIL: Map for effect.
SET: Return a fresh set containing the mapped elements.
LIST: Return a fresh list containing the mapped elements in arbitrary order.
(Note that \"arbitrary\" does not mean \"random\".

See DOSET"
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

(defun set-to-list (set)
  "Return a fresh list containing the elements of SET in arbitrary order.
(Note that \"arbitrary\" does not mean \"random\"."
  (mapset 'list #'identity set))

(defun copy-set (set)
  "Return a fresh set containing all the elements of SET."
  #+hashset
  (let ((result (hashset)))
    (doset (e set (make-instance 'set :hash result))
      (setf (gethash e result) t)))
  #+listset
  (make-instance 'set :list (copy-list (%setlist set))))

(defun arb (set)
  "Return an arbitrary element of SET.
If SET has no elements, return no values."
  (doset (i set (values)) (return-from arb i)))

(defun set<= (set1 set2)
  "Return true iff SET1 is a subset of SET2, i.e. SET2 contains all elements SET1 does."
  (doset (i set1 t)
    (unless (presentp i set2) (return nil))))

(defun set= (set1 set2)
  "Return true iff SET1 and SET2 contain the same elements."
  (and (= (size set1) (size set2))
       (doset (i set1 t)
         (unless (presentp i set2) (return nil)))))

(defun some (predicate set)
  "Return true iff some element of SET is true under PREDICATE.
Note that unlike CL:SOME, this does not necessarily return the result of the predication."
  (doset (i set nil)
    (when (funcall predicate i) (return t))))

(defun every (p set)
  "Return true iff every element of SET is true under PREDICATE."
  (doset (i set t)
    (unless (funcall p i) (return nil))))

(defun notany (p set)
  "Return true iff no elements of SET are true under PREDICATE."
  (doset (i set t)
    (when (funcall p i) (return nil))))

(defun notevery (p set)
  "Return true iff some element of SET is not true under PREDICATE."
  (doset (i set nil)
    (unless (funcall p i) (return t))))

(defun nadjoin (item set)
  "Return a set with all the elements of SET plus ITEM, destroying SET in the process."
  #+hashset
  (setf (gethash item (%hash set)) t)
  #+listset
  (pushnew item (%setlist set))
  set)

;;; define-modify-macro reorders arguments and i don't like it
(defmacro nadjoinf (set item &environment env)
  "Set the SET place to a set with the elements of SET plus ITEM, destroying the original set in the process.

See NADJOIN"
  (multiple-value-bind (temps values stores write read)
      (get-setf-expansion set env)
    `(let* (,@(mapcar #'list temps values))
       (multiple-value-bind (,@stores) (nadjoin ,item ,read)
         ,write))))

(defun nremove (item set)
  "Return a set with all the elements of SET sans ITEM, destroying SET in the process."
  #+hashset
  (remhash item (%hash set))
  #+listset
  (setf (%setlist set) (delete item (%setlist set) :count 1))
  set)

(defmacro nremovef (set item &environment env)
  "Set the SET place to a set with the elements of SET sans ITEM, destroying the original set in the process."
  (multiple-value-bind (temps values stores write read)
      (get-setf-expansion set env)
    `(let* (,@(mapcar #'list temps values))
       (multiple-value-bind (,@stores) (nremove ,item ,read)
         ,write))))

(defun nset-empty (set)
  "Return an empty set. SET is destroyed."
  #+hashset
  (clrhash (%hash set))
  #+listset
  (setf (%setlist set) nil)
  set)

(defun nunion (s1 s2)
  "Return the union of S1 and S2. S1 is destroyed.

See UNION"
  (doset (i s2 s1) (nadjoinf s1 i)))

(defun union (s1 s2)
  "Return the union of S1 and S2."
  (let ((result (empty-set)))
    (doset (i s1) (nadjoinf result i))
    (doset (i s2) (nadjoinf result i))
    result))

(defmacro nunionf (set other &environment env)
  "Set the SET place to be the union of SET and OTHER, destroying the original SET in the process.

See NUNION"
  (multiple-value-bind (temps values stores write read)
      (get-setf-expansion set env)
    `(let* (,@(mapcar #'list temps values))
       (multiple-value-bind (,@stores) (nunion ,read ,other)
         ,write))))

(defun filter (result-type predicate set)
  "Return an object containing all the elements of SET that are true under the PREDICATE.
RESULT-TYPE may be SET or LIST."
  (ecase result-type
    (set
     (let ((result (empty-set)))
       (doset (e set result)
         (when (funcall predicate e) (nadjoinf result e)))))
    (list
     (let ((result nil))
       (doset (e set result)
         (when (funcall predicate e) (push e result)))))))

(defun difference (result-type minuend subtrahend)
  "Return an object containing all the elements of MINUEND that are not present in SUBTRAHEND.
RESULT-TYPE may be SET or LIST."
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
  "Return a set containing all elements of MINUEND not in SUBTRAHEND, destroying MINUEND in the process."
  (doset (s subtrahend) (nremovef minuend s)))

(defmacro nsubtractf (minuend subtrahend &environment env)
  "Set the MINUEND place to be the difference of MINUEND and SUBTRAHEND, destroying MINUEND in the process.

See NSUBTRACT"
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
