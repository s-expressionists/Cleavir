(in-package #:cleavir-example)

(defun load-environment (&optional (environment *environment*))
  "Fill the given example environment full of much of the CL package, taken from the host.
Note that most macros are not taken, since they may not expand into portable code."
  (do-external-symbols (s "CL")
    (cond ((constantp s)
           (%defconstant s (cl:eval s) environment))
          (t (multiple-value-bind (expansion expandedp)
                 (macroexpand-1 s)
               (when expandedp
                 (%defsmacro s expansion environment)))))
    (cond ((and (fboundp s)
                (not (macro-function s))
                (not (special-operator-p s)))
           (%defun s (fdefinition s) environment))
          ((not (member s *functions*))
           (let ((pair (assoc s *macros*)))
             (when pair
               (%defmacro s (cdr pair) environment)))))
    (cond ((find-class s nil)
           (%defclass s (find-class s) environment))))
  (loop for f in *functions* do
    (%defun f (fdefinition f) environment))
  ;; Force computing a policy
  (proclaim-optimize '((speed 1) (compilation-speed 1)
                       (debug 1) (space 1) (safety 1))
                     environment)
  (values))
