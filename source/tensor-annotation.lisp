
(in-package :cl-onnx)

(define-proto (tensor-annotation cl-protobufs.onnx:tensor-annotation)
	      (tensor-name string nil t nil)
	      (quant-parameter-tensor-names string-string-entry-proto t t t))

