
(in-package :cl-onnx)

(define-proto (function-proto cl-protobufs.onnx:function-proto)
	      (name string nil t nil)
	      (input string nil t t)
	      (output string nil t t)
	      (attribute string nil t t)
	      (attribute-proto attribute-proto t t t)
	      (node node-proto t t t)
	      (doc-string string nil t nil)
	      (opset-import operator-set-id-proto t t t)
	      (domain string nil t nil)
	      (overload string nil t nil)
	      (value-info value-info-proto t t t)
	      (metadata-props string-string-entry-proto t t t))

