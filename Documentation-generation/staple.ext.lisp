(defpackage #:cleavir-documentation-generation
  (:use #:cl))

(in-package #:cleavir-documentation-generation)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; TREE-PROJECT
;;;
;;; This is a subclass of STAPLE:PROJECT geared towards a system composed of
;;; subsystems, each of which may be configured individually. The tree of
;;; subsystems may have arbitrary depth (e.g. subsubsubsystems are ok).
;;; Each asdf system is represented by its own TREE-PROJECT, and a
;;; corresponding subproject for each subsystem.

(defclass tree-project (staple:project)
  (;; ASDF system for this project.
   (%system :initarg :system :reader staple:system :type asdf:system)
   ;; Parent project, or NIL if this is the top of the hierarchy.
   (%parent :initarg :parent :reader parent :type (or staple:project null))
   ;; List of PROJECTs.
   (%subprojects :initarg subprojects :reader subprojects
                 :accessor %subprojects :type list)
   ;; Output directory.
   (%output :initarg :output :reader staple:output)
   ;; This is the list of "local" pages, i.e. pages for this project
   ;; but not its subprojects.
   (%pages :initarg :pages :initform nil :accessor staple:pages)))

(defmethod print-object ((o tree-project) s)
  (print-unreadable-object (o s :type t)
    (ignore-errors (write (asdf:component-name (staple:system o)) :stream s)))
  o)

(defmethod staple:generate ((project tree-project) &rest args)
  (let ((generated-pages ()) (generated-subprojects ()))
    ;; Generate pages.
    (with-simple-restart (abort "Abort ~a" project)
      (dolist (page (staple:pages project))
        (with-simple-restart (continue "Ignore ~a" page)
          (push (apply #'staple:generate page args) generated-pages))))
    ;; Generate subprojects.
    (with-simple-restart (abort "Abort ~a" project)
      (dolist (subproject (subprojects project))
        (with-simple-restart (continue "Ignore ~a" subproject)
          (push (apply #'staple:generate subproject args) generated-subprojects))))
    (values project (nreverse generated-pages) (nreverse generated-subprojects))))

;;; Make a TREE-PROJECT corresponding to the given system.
(defgeneric tree-project (system parent &key &allow-other-keys))
(defmethod tree-project ((system asdf:system) parent
                         &key output-directory images documents page-type template packages subsystems)
  (staple:load-extension system)
  (let* ((page-type (or page-type (staple:page-type system)))
         (template (or template (staple:template system)))
         (images (or images (staple:images system)))
         (packages (or packages (staple:packages system)))
         (output-directory
           (or output-directory
               (and parent
                    (pathname-utils:subdirectory (staple:output parent)
                                                 (asdf:component-name system)))
               (staple:output-directory system)))
         (project
           (make-instance 'tree-project
             :parent parent :system system
             :output output-directory)))
    ;; This dummy (nil) ensures that we get a blank index page
    ;; even if there is no README. (A README should probably be added though.)
    (loop for doc in (or documents (staple:documents system) '(nil))
          for page = (make-instance page-type
                       :project project
                       :input template
                       :output output-directory
                       :system system
                       :document doc
                       :images images
                       :packages packages)
          do (push page (staple:pages project)))
    (loop for image in images
          for page = (make-instance 'staple:static-page
                       :project project
                       :input image
                       :output (pathname-utils:file-in output-directory image))
          do (push page (staple:pages project)))
    (setf (%subprojects project)
          (loop for subspec in (or subsystems (staple:subsystems system))
                collect (destructuring-bind (subsys . args)
                            (if (listp subspec) subspec (list subspec))
                          (apply #'tree-project subsys project args))))
    project))

(defmethod clip:clip ((object tree-project) (field (eql :name)))
  (asdf:component-name (staple:system object)))

(defmethod clip:clip ((object tree-project) (field (eql :description)))
  (asdf:system-description (staple:system object)))

(defmethod clip:clip ((object tree-project) (field (eql :root-html)))
  (format nil "~aindex.html"
          (pathname-utils:relative-pathname
           (staple:output (parent object)) (staple:output object))))

(defmethod clip:clip ((object tree-project) (field (eql :title)))
  ;; KLUDGE
  (staple::titleize (asdf:component-name (staple:system object))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SYSTEM-INDEX-PAGE
;;;
;;; A subclass of STAPLE:PAGE that's built to list out subprojects.

(defclass system-index-page (staple:simple-page)
  ())

(defmethod staple:template-data append ((page system-index-page))
  (list :description "Compiler toolkit for Lisp." ; KLUDGE
        :subprojects (subprojects (staple:project page))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Particularities for Cleavir
;;;

;;; This is just a SIMPLE-PAGE, but set up to fix source links correctly
;;; and display subprojects.
(defclass cleavir-page (staple:simple-page) ())

(defparameter *cleavir-root* (pathname-utils:parent
                              (asdf:system-source-directory
                               :cleavir-documentation-generation)))

(defparameter *cleavir-home* "https://github.com/s-expressionists/Cleavir")

(defparameter *cleavir-main-branch* "main")

(defmethod staple:resolve-source-link (source (page cleavir-page))
  (if (pathname-utils:subpath-p (truename (getf source :file)) (truename *cleavir-root*))
      (format nil "~a/blob/~a/~a~@[#L~a~]"
              *cleavir-home* *cleavir-main-branch*
              (enough-namestring (getf source :file) *cleavir-root*)
              ;; Line numbers seem to be totally broken. Not sure why yet.
              nil #+(or)(getf source :row))
      (call-next-method)))

(defmethod staple:template-data append ((page cleavir-page))
  (list :subprojects (subprojects (staple:project page))))

(defmethod staple:subsystems ((system (eql (asdf:find-system :cleavir-documentation-generation))))
  (mapcar #'asdf:find-system
          '(:cleavir-example
            :cleavir-cst-to-ast
            :cleavir-environment
            :cleavir-ast
            :cleavir-ast-to-bir
            :cleavir-bir
            :cleavir-set
            :cleavir-attributes
            :cleavir-ctype
            :cleavir-primop
            :cleavir-compilation-policy
            :cleavir-conditions
            :cleavir-io
            :cleavir-meter
            :cleavir-stealth-mixins)))

;;; Use global README, not the one in this directory. The README in this directory is not
;;; actually part of Cleavir's documentation, as it just explains the generator.
(defmethod staple:documents ((system (eql (asdf:find-system :cleavir-documentation-generation))))
  (list (asdf:system-relative-pathname system "../README.md")))

(defmethod staple:output-directory ((system (eql (asdf:find-system :cleavir-documentation-generation))))
  (asdf:system-relative-pathname system "../docs/"))

(defmethod staple:template ((system (eql (asdf:find-system :cleavir-documentation-generation))))
  (asdf:system-relative-pathname system "top" :type "ctml"))

(defmethod staple:packages ((system (eql (asdf:find-system :cleavir-documentation-generation))))
  ;; Don't generate any code for this dummy system.
  ())

(defmethod staple:page-type ((system (eql (asdf:find-system :cleavir-documentation-generation))))
  'system-index-page)

(defmethod staple:find-project ((sys (eql (asdf:find-system :cleavir-documentation-generation)))
                                &rest keys &key &allow-other-keys)
  (let ((proj (apply #'tree-project sys nil keys)))
    ;; Set the title of the index page manually.
    (setf (staple:title (first (staple:pages proj))) "Cleavir")
    proj))
