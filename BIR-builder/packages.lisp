(defpackage #:cleavir-bir-builder
  (:use #:cl)
  (:local-nicknames (#:bir #:cleavir-bir)
                    (#:set #:cleavir-set))
  (:export #:inserter #:constant #:vcell #:fcell #:adjoin-variable)
  (:export #:begin #:proceed #:insert #:terminate)
  (:export #:make-iblock))
