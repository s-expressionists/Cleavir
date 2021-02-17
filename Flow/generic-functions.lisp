(in-package #:cleavir-flow)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function FLOW.
;;;
;;; Compute the new information at the given graph node, and if necessary,
;;; mark other nodes for processing.
;;; In order for the analysis process to halt at a fixed point, nodes must not
;;; be marked if there is no more information to compute for them.
;;; Must be defined for each flow class.
;;; FLOW is not intended to be called by programmers; they may write methods.
;;;

(defgeneric flow (flow graph node))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function MARK.
;;;
;;; While performing a dataflow analysis, mark the given node for processing.
;;; Only needs to be customized if you are writing your own traversal class.
;;; For convenience, if called with a flow, MARK will use the flow's traversal.
;;; Programmers should only call MARK during a FLOW method.
;;;

(defgeneric mark (traversal node))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function WORK.
;;;
;;; Do the work of traversing, i.e. iterate through the graph until a fixed
;;; point is reached.
;;; Only needs to be customized if you are writing your own traversal class.
;;; WORK is not intended to be called by programmers; they may write methods.
;;;

(defgeneric work (traversal graph))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function INITIALIZE.
;;;
;;; Make a traversal ready for WORK. This should involve marking nodes to start
;;; the analysis from.
;;; The default method marks all nodes, unless the traversal has a :DIRECTION
;;; of :FORWARD, in which case it only marks the graph root.
;;; INITIALIZE is not intended to be called by programmers; they may write
;;; methods.
;;;

(defgeneric initialize (traversal graph))
