(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-abstract-interpreter
  (:use #:cl)
  (:local-nicknames (#:bir #:cleavir-bir)
                    (#:set #:cleavir-set)
                    (#:ctype #:cleavir-ctype)
                    (#:attributes #:cleavir-attributes))
  (:shadow #:type)
  (:export #:domain
           #:infimum #:supremum #:subinfop #:join/2 #:meet/2 #:wjoin/2
           #:meet #:join #:wjoin)
  (:export #:data #:forward-data #:backward-data
           #:values-domain #:forward-values-data #:backward-values-data
           #:control #:forward-control)
  (:export #:sv-infimum #:sv-supremum #:sv-subinfop
           #:sv-join/2 #:sv-meet/2 #:sv-wjoin/2
           #:values-info #:values-required #:values-optional #:values-rest
           #:info-values-nth #:primary #:single-value)
  (:export #:flow-datum #:flow-datum-through)
  (:export #:product #:product-domain #:product-domain-of-type)
  (:export #:strategy #:optimism #:pessimism
           #:mark #:flow-call #:interpret-module #:interpret-instruction
           #:info)
  (:export #:attribute)
  (:export #:type #:derived-type #:asserted-type
           #:derive-return-type)
  (:export #:reachability)
  (:export #:sequential)
  (:export #:slots #:sequential-slots))
