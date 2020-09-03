(in-package #:cleavir-bir)

(deftype set () 'hash-table)

(defun empty-set ()
  ;; FIXME: should be weak-key
  (make-hash-table :test #'eq))

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
            (unless itemp (return nil))
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
