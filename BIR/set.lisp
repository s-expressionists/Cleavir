(in-package #:cleavir-bir)

;;; hash sets, with a simple wrapper so it prints nicely
(defclass set ()
  ((%hash :initarg :hash :reader %hash)))

;; internal to this file
;; FIXME: should be weak key
(declaim (inline hashset))
(defun hashset () (make-hash-table :test #'eq))

(defun empty-set () (make-instance 'set :hash (hashset)))

(defun empty-set-p (set)
  (zerop (hash-table-count (%hash set))))

(defun size (set)
  (hash-table-count (%hash set)))

(defun make-set (&rest elements)
  (loop with r = (hashset)
        for e in elements
        do (setf (gethash e r) t)
        finally (return (make-instance 'set :hash r))))

(defun presentp (item set)
  (values (gethash item (%hash set))))

(defmacro doset ((var setform &optional result) &body body)
  `(loop for ,var being the hash-keys of (%hash ,setform)
         do (tagbody ,@body)
         finally (return ,result)))

(defun mapset (result-type f set)
  (ecase result-type
    ((nil) (doset (item set nil) (funcall f item)))
    ((set) (let ((new (hashset)))
             (doset (item set (make-instance 'set :hash new))
               (setf (gethash (funcall f item) new) t))))
    ((list) (let (r) (doset (item set r) (push (funcall f item) r))))))

(defun set-to-list (set) (mapset 'list #'identity set))

(defun copy-set (set)
  (let ((result (hashset)))
    (doset (e set (make-instance 'set :hash result))
      (setf (gethash e result) t))))

;;; Get an arbitrary element of the set, or else no values.
(defun arb (set)
  (doset (i set (values)) (return-from arb i)))

(defun set= (set1 set2)
  (and (= (size set1) (size set2))
       (doset (i set1 t)
         (unless (presentp i set2) (return nil)))))

(defun every (p set)
  (doset (i set t)
    (unless (funcall p i) (return nil))))

(defun nadjoin (item set)
  (setf (gethash item (%hash set)) t)
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
  (remhash item (%hash set))
  set)

(defmacro nremovef (set item &environment env)
  (multiple-value-bind (temps values stores write read)
      (get-setf-expansion set env)
    `(let* (,@(mapcar #'list temps values))
       (multiple-value-bind (,@stores) (nremove ,item ,read)
         ,write))))

;; Return an empty set, possibly destroying an existing set to do it.
(defun nset-empty (set)
  (clrhash (%hash set))
  set)

;; Only the first argument is destroyed.
(defun nunion (s1 s2)
  (doset (i s2 s1) (nadjoinf s1 i)))

(defmacro nunionf (set other &environment env)
  (multiple-value-bind (temps values stores write read)
      (get-setf-expansion set env)
    `(let* (,@(mapcar #'list temps values))
       (multiple-value-bind (,@stores) (nunion ,read ,other)
         ,write))))

(defun filter (f set)
  (let ((result (empty-set)))
    (doset (e set result)
      (when (funcall f e) (nadjoinf result e)))))

(defmethod print-object ((s set) stream)
  (print-unreadable-object (s stream :type t)
    (format stream "{~{~a~^ ~}}" (set-to-list s))))
