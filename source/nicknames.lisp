
(in-package :cl-onnx)

(named-readtables:in-readtable cl-annot-revisit:at-syntax-readtable)

;; Nicknames for each accessors

@cl-annot-revisit:export
(defmethod graph ((proto Model-Proto))
  "A shortcut of Model-Proto -> Grpah"
  (model-proto-graph proto))

;; (defmethod shape ((proto
