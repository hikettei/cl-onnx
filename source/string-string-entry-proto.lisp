
(in-package :cl-onnx)

(define-proto (string-string-entry-proto cl-protobufs.onnx:string-string-entry-proto)
	      ;; (name type onnxp optionalp listp)
	      (key string nil t nil)
	      (value string nil t nil))


