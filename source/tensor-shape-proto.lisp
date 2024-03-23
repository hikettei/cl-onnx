
(in-package :cl-onnx)

(define-proto (tensor-shape-proto cl-protobufs.onnx:tensor-shape-proto)
	      (dim t nil t t)) ;; tensor-shape-proto.dimension
