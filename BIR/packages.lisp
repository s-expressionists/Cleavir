(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-bir
  (:use #:cl)
  (:shadow #:function #:catch #:set #:variable #:load-time-value #:case
           #:disassemble)
  (:export #:module #:functions)
  (:export #:function #:iblocks #:start #:end #:inputs #:variables #:catches
           #:environment
           #:local-calls #:lambda-list #:name #:docstring #:original-lambda-list)
  (:export #:dynamic-environment #:scope #:parent #:bindings)
  (:export #:iblock #:predecessors #:entrances #:exits #:iblock-started-p)
  (:export #:rtype #:rtype=)
  (:export #:datum #:ssa #:value #:linear-datum #:transfer #:argument #:phi
           #:output #:name #:ctype #:ctyped-p
           #:definitions #:uses #:definition #:use #:unused-p)
  (:export #:variable #:extent #:writers #:readers #:encloses #:binder
           #:immutablep #:closed-over-p)
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
           #:abstract-call #:callee #:call #:local-call #:mv-call #:attributes #:transforms
           #:leti #:dynamic-leti #:enclose #:code)
  (:export #:primop-info #:in-rtypes #:out-rtypes #:defprimop
           #:primop #:vprimop #:nvprimop #:tprimop #:info)
  (:export #:map-iblocks
           #:insert-instruction-before #:insert-instruction-after
           #:replace-computation #:delete-computation #:delete-instruction
           #:delete-transmission #:replace-uses #:replace-terminator
           #:split-block-after #:delete-iblock #:maybe-delete-iblock
           #:clean-up-iblock #:iblocks-mergable-p #:merge-iblocks
           #:remove-function-from-module)
  (:export #:refresh-iblocks #:refresh-local-iblocks
           #:refresh-users #:refresh-local-users)
  (:export #:verify)
  (:export #:disassemble))
