#+(or)
(defclass compound-page (staple:simple-page) ())

(defmethod staple:subsystems ((system (eql (asdf:find-system :cleavir-bir))))
  (list (asdf:find-system :cleavir-bir-visualizer)))

#+(or)
(defmethod staple:filename ((page compound-page))
  (let* ((doc (staple:document page))
         (name (pathname-name doc)))
    (if (string= name "README")
        (call-next-method) ; index.html
        (make-pathname :name name :type "html"))))

#+(or)
(defmethod staple:page-type ((system (eql (asdf:find-system :cleavir-bir))))
  'compound-page)

#+(or)
(defmethod staple:documents ((system (eql (asdf:find-system :cleavir-bir))))
  ;; Gather all markdown files as documents.
  (let ((staple:*document-patterns* (list* ".*\\.md" staple:*document-patterns*)))
    (call-next-method)))

;;; Stupid hack
(defmethod staple:output-directory ((system (eql (asdf:find-system :cleavir-bir))))
  (asdf:system-relative-pathname system "../docs/"))
