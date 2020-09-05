(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-bir
  (:use #:cl)
  (:shadow #:function #:catch #:set #:variable #:load-time-value #:case)
  (:export #:function #:iblocks #:start #:end #:inputs #:variables)
  (:export #:iblock #:predecessors #:entrances #:dynamic-environment)
  (:export #:datum #:value #:linear-datum #:argument
           #:definitions #:uses)
  (:export #:variable #:extent #:owner #:writers #:readers #:encloses)
  (:export #:constant #:make-constant #:immediate #:load-time-value
           #:constant-value #:immediate-value #:form #:read-only-p)
  (:export #:instruction #:operation #:computation
           #:terminator #:terminator0 #:terminator1
           #:successor #:predecessor #:next)
  (:export #:multiple-to-fixed #:fixed-to-multiple #:extract #:create
           #:accessvar #:writevar #:readvar
           #:returni #:unreachable #:eqi #:jump #:local-unwind
           #:typeq #:type-specifier
           #:case #:comparees
           #:catch #:unwinds #:unwind #:destination
           #:alloca #:writetemp #:readtemp
           #:call #:enclose)
  (:export #:primop-info #:in-rtypes
           #:primop #:vprimop #:nvprimop)
  (:export #:rtype #:rtype=
           #:aggregate #:make-aggregate #:aggregatep
           #:aggregate-length #:aggregate-elt)
  (:export #:refresh-iblocks #:refresh-local-iblocks
           #:refresh-users #:refresh-local-users)
  (:export #:verify)
  (:export #:make-set #:nset-adjoin #:nset-adjoinf))
