
(in-package :cl-onnx)

(define-proto (training-info-proto cl-protobufs.onnx:training-info-proto)
	      ;; name type onnxp optionalp listp
	      (initialization graph-proto t t nil)
	      (algorithm graph-proto t t nil)
	      (initialization-binding string-string-entry-proto t t t)
	      (update-binding string-string-entry-proto t t t))

