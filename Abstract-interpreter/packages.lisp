(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-abstract-interpreter
  (:use #:cl)
  (:local-nicknames (#:bir #:cleavir-bir)
                    (#:set #:cleavir-set)
                    (#:ctype #:cleavir-ctype)
                    (#:attributes #:cleavir-attributes))
  (:shadow #:type)
  (:export #:domain
           #:infimum #:supremum #:subinfop #:join/2 #:meet/2
           #:meet #:join #:widen)
  (:export #:data #:forward-data #:backward-data
           #:values-domain #:forward-values-data #:backward-values-data
           #:control #:forward-control)
  (:export #:sv-infimum #:sv-supremum #:sv-subinfop
           #:sv-join/2 #:sv-meet/2 #:sv-widen
           #:values-info #:values-required #:values-optional #:values-rest
           #:info-values-nth #:primary #:single-value)
  (:export #:product #:channel #:scalar-channel #:coop-channel)
  (:export #:strategy #:optimism #:pessimism
           #:mark #:interpret-module #:flow-instruction
           #:info)
  (:export #:attribute)
  (:export #:known-call-channel #:flow-known-call)
  (:export #:type #:derived-type #:asserted-type)
  (:export #:reachability)
  (:export #:reachability->data #:type->reachability)
  (:export #:sequential)
  (:export #:slots #:sequential-slots))
