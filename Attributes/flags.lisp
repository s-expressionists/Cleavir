(in-package #:cleavir-attributes)

;;; We represent boolean flags as an integer bitfield.

(defun make-flags (&rest flags)
  "Return computed flags, given a list of flag specifiers (keywords like :NO-CALL).

See HAS-FLAG-P"
  (let ((result 0))
    (dolist (flag flags)
      (let ((bits (ecase flag
                    ((:no-call) #b11)
                    ((:dyn-call) #b10)
                    ((:dx-call) #b100)
                    ((:flushable) #b1000))))
        (setf result (logior result bits))))
    result))

(defun sub-flags-p (flags1 flags2) (zerop (logandc2 flags1 flags2)))

(defun meet-flags (flags1 flags2) (logand flags1 flags2))
(defun join-flags (flags1 flags2) (logior flags1 flags2))

(defun %has-flag-p (flags flag-name)
  (logbitp
   (ecase flag-name
     ((:no-call) 0)
     ((:dyn-call) 1)
     ((:dx-call) 2)
     ((:flushable) 3))
   flags))
