(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-ast
  (:use #:common-lisp)
  (:shadow #:symbol #:ignore)
  (:export
   #:ast
   #:define-children #:children #:map-children
   #:source-info
   #:origin
   #:*policy* #:policy
   #:constant-ast #:make-constant-ast #:value
   #:lexical-bind-ast #:make-lexical-bind-ast #:ignore
   #:lexical-ast #:make-lexical-ast
   #:constant-dynamic-bind-ast
   #:set-constant-symbol-value-ast #:make-set-constant-symbol-value-ast
   #:symbol #:symbol-ast
   #:info #:name-ast
   #:constant-fdefinition-ast #:make-constant-fdefinition-ast #:info
   #:constant-symbol-value-ast #:make-constant-symbol-value-ast #:info
   #:call-ast #:make-call-ast #:callee-ast #:argument-asts
   #:inline-declaration #:attributes
   #:primop-ast
   #:block-ast #:make-block-ast #:body
   #:function-ast #:make-function-ast #:lambda-list
   #:bound-declarations #:docstring #:original-lambda-list
   #:top-level-function-ast #:make-top-level-function-ast #:forms
   #:body-ast
   #:inline-ast
   #:go-ast #:make-go-ast #:tag-ast
   #:if-ast #:make-if-ast #:test-ast #:then-ast #:else-ast
   #:branch-ast #:make-branch-ast #:branch-asts #:default-ast
   #:multiple-value-call-ast #:make-multiple-value-call-ast
   #:function-form-ast
   #:multiple-value-prog1-ast #:make-multiple-value-prog1-ast
   #:first-form-ast
   #:load-time-value-ast #:make-load-time-value-ast #:read-only-p
   #:form
   #:progn-ast #:make-progn-ast #:form-asts
   #:return-from-ast #:make-return-from-ast #:form-ast
   #:setq-ast #:make-setq-ast #:value-ast
   #:tagbody-ast #:make-tagbody-ast #:prefix-ast #:item-asts
   #:tag-ast #:make-tag-ast #:name
   #:unwind-protect-ast #:cleanup-ast
   #:the-ast #:make-the-ast #:ctype
   #:typeq-ast #:make-typeq-ast #:test-ctype
   #:type-check-function-ast
   #:eq-ast #:make-eq-ast #:arg1-ast #:arg2-ast
   #:case-ast #:make-case-ast #:arg-ast #:comparees
   #:lexical-variable #:make-lexical-variable
   #:unreachable-ast #:make-unreachable-ast
   #:child-ast
   #:map-ast-depth-first-preorder
   ))

(defpackage #:cleavir-ast-graphviz
  (:use #:common-lisp #:cleavir-ast)
  (:shadowing-import-from #:cleavir-ast #:symbol #:ignore)
  (:export
   #:draw-ast))
