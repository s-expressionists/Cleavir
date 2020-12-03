(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-bir
  (:use #:cl)
  (:shadow #:function #:catch #:set #:variable #:load-time-value #:case
           #:disassemble #:ignore)
  (:export #:module #:functions #:constants #:constant-in-module #:load-time-values)
  (:export #:function #:iblocks #:start #:end #:inputs #:variables #:catches
           #:environment
           #:local-calls #:lambda-list #:name #:docstring #:original-lambda-list)
  (:export #:dynamic-environment #:scope #:parent)
  (:export #:iblock #:predecessors #:entrances #:exits #:iblock-started-p #:deletedp)
  (:export #:rtype #:rtype=)
  (:export #:datum #:ssa #:value #:linear-datum #:transfer #:argument #:phi
           #:output #:name #:ctype #:assert-type-on-linear-datum #:derive-type-for-linear-datum
           #:definitions #:use #:transitive-use #:unused-p)
  (:export #:variable #:extent #:writers #:readers #:encloses #:binder
           #:use-status #:ignore
           #:record-variable-ref #:record-variable-set
           #:immutablep #:closed-over-p)
  (:export #:constant #:load-time-value
           #:constant-value #:form #:read-only-p)
  (:export #:instruction #:operation #:computation #:inputs #:outputs
           #:terminator #:terminator0 #:terminator1
           #:successor #:predecessor #:next
           #:origin #:policy)
  (:export #:*origin* #:*policy*)
  (:export #:multiple-to-fixed #:fixed-to-multiple
           #:accessvar #:writevar #:readvar #:cast
           #:constant-reference #:make-constant-reference
           #:returni #:unreachable #:eqi #:jump #:unwindp
           #:eq-test #:typeq-test
           #:ifi #:conditional-test
           #:type-specifier #:typew #:ctype #:choke
           #:case #:comparees
           #:catch #:unwinds #:unwind #:destination
           #:alloca #:writetemp #:readtemp
           #:abstract-call #:callee #:call #:local-call
           #:abstract-local-call #:mv-call #:mv-local-call
           #:attributes #:transforms
           #:leti #:dynamic-leti #:enclose #:code)
  (:export #:primop #:vprimop #:nvprimop #:tprimop #:info)
  (:export #:map-iblock-instructions #:map-iblock-instructions-backwards
           #:do-iblock-instructions)
  (:export #:map-iblocks #:map-local-instructions
           #:insert-instruction-before #:insert-instruction-after
           #:replace-computation #:delete-computation #:delete-instruction
           #:delete-transmission #:replace-uses #:replace-terminator
           #:split-block-after #:delete-iblock #:maybe-delete-iblock
           #:clean-up-iblock #:merge-successor-if-possible)
  (:export #:refresh-iblocks #:refresh-local-iblocks
           #:refresh-users #:refresh-local-users)
  (:export #:verify)
  (:export #:disassemble)
  (:export #:unused-variable))
