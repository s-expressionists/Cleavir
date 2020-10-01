(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-bir
  (:use #:cl)
  (:shadow #:function #:catch #:set #:variable #:load-time-value #:case
           #:disassemble)
  (:export #:module #:functions)
  (:export #:function #:iblocks #:start #:end #:inputs #:variables
           #:lambda-list #:name #:docstring #:original-lambda-list)
  (:export #:dynamic-environment #:scope #:parent #:lexical-bind #:bindings)
  (:export #:iblock #:predecessors #:entrances #:exits #:iblock-started-p)
  (:export #:rtype #:rtype=)
  (:export #:datum #:ssa #:value #:linear-datum #:transfer #:argument #:phi
           #:output #:name
           #:definitions #:uses #:definition #:use)
  (:export #:variable #:extent #:writers #:readers #:encloses #:binder #:immutablep #:closed-over-p)
  (:export #:constant #:make-constant #:immediate #:load-time-value
           #:constant-value #:immediate-value #:form #:read-only-p)
  (:export #:instruction #:operation #:computation #:inputs #:outputs
           #:terminator #:terminator0 #:terminator1
           #:successor #:predecessor #:next
           #:origin #:policy)
  (:export #:*origin* #:*policy*)
  (:export #:multiple-to-fixed #:fixed-to-multiple
           #:accessvar #:writevar #:readvar #:cast
           #:returni #:unreachable #:eqi #:jump #:unwindp
           #:typeq #:type-specifier #:typew #:ctype #:choke
           #:case #:comparees
           #:catch #:unwinds #:unwind #:destination
           #:alloca #:writetemp #:readtemp
           #:leti #:call #:mv-call #:enclose #:code)
  (:export #:primop-info #:in-rtypes #:out-rtypes #:defprimop
           #:primop #:vprimop #:nvprimop)
  (:export #:map-instructions #:map-instructions-with-owner-from-set
           #:map-iblocks
           #:insert-instruction-before #:insert-instruction-after
           #:replace-computation #:delete-computation #:delete-instruction
           #:replace-uses #:replace-terminator #:split-block-after
           #:move-inputs #:delete-iblock #:maybe-delete-iblock)
  (:export #:refresh-iblocks #:refresh-local-iblocks
           #:refresh-users #:refresh-local-users)
  (:export #:verify)
  (:export #:disassemble))
