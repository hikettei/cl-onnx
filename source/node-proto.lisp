
(in-package :cl-onnx)

(define-proto (node-proto cl-protobufs.onnx:node-proto)
	      ;; name type onnxp optionalp listp
	      (input string nil t t)
	      (output string nil t t)
	      (name string nil t nil)
	      (op-type string nil t nil)
	      (domain string nil t nil)
	      (overload string nil t nil)
	      (attribute attribute-proto t t t)
	      (doc-string string nil t nil)
	      (metadata-props string-string-entry-proto t t t))
