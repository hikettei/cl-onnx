
(in-package :cl-onnx)

(define-proto (graph-proto cl-protobufs.onnx:graph-proto)
	      (node node-proto t t t)
	      (name string nil t nil)
	      (initializer tensor-proto t t t)
	      (sparse-initializer sparse-tensor-proto t t t)
	      (doc-string string nil t nil)
	      (input value-info-proto t t t)
	      (output value-info-proto t t t)
	      (value-info value-info-proto t t t)
	      (quantization-annotation tensor-annotation t t t)
	      (metadata-props string-string-entry-proto t t t))
