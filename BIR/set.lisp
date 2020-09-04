(in-package #:cleavir-bir)

(deftype set () 'hash-table)

(defun empty-set ()
  ;; FIXME: should be weak-key
  (make-hash-table :test #'eq))

(defun empty-set-p (set)
  (zerop (hash-table-count set)))

(defun make-set (&rest elements)
  (loop with r = (empty-set)
        for e in elements
        do (setf (gethash e r) t)
        finally (return r)))

(defun presentp (item set)
  (values (gethash item set)))

(defun mapset (f set)
  (with-hash-table-iterator (it set)
    (loop (multiple-value-bind (itemp item) (it)
            (unless itemp (return (values)))
            (funcall f item)))))

(defun set= (set1 set2)
  (and (= (hash-table-count set1) (hash-table-count set2))
       (block nil
         (mapset (lambda (i) (unless (presentp i set2) (return nil)))
                 set1)
         t)))

(defun set-every (p set)
  (mapset (lambda (i) (unless (funcall p i) (return-from set-every nil))) set)
  t)

(defun nset-adjoin (item set)
  (setf (gethash item set) t)
  set)

;;; define-modify-macro reorders arguments and i don't like it
(defmacro nset-adjoinf (set item &environment env)
  (multiple-value-bind (temps values stores write read)
      (get-setf-expansion set env)
    `(let* (,@(mapcar #'list temps values))
       (multiple-value-bind (,@stores) (nset-adjoin ,item ,read)
         ,write))))

;; Removes an item from the set, returns the new set. Can be (is) destructive.
;; could be set-delete, but n is regular, if arbitrary
(defun nset-remove (item set)
  (remhash item set)
  set)

;; Return an empty set, possibly destroying an existing set to do it.
(defun nset-empty (set)
  (clrhash set)
  set)
