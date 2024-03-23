
(in-package :cl-onnx)

;; (name type onnx-p optionalp listp)
(define-proto (attribute-proto cl-protobufs.onnx:attribute-proto)
	      (name string nil t nil)
	      (ref-attr-name string nil t nil)
	      (doc-string string nil t nil)
	      (type keyword nil t nil)
	      (f single-float nil t nil nil)
	      (i fixnum nil t nil nil)
	      (s (signed-byte 32) nil t nil nil)
	      (g graph-proto t t nil)
	      (sparse-tensor sparse-tensor-proto t t nil)
	      (tp type-proto t t nil)
	      (floats list nil t t)
	      (ints list nil t t)
	      (strings list nil t t)
	      (tensors list t t t)
	      (graphs list t t t)
	      (type-protos list t t t))
