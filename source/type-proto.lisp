
(in-package :cl-onnx)

(define-proto (type-proto cl-protobufs.onnx:type-proto)
	      (tensor-type t nil t nil)
	      (sequence-type t nil t nil)
	      (map-type t nil t nil)
	      (sparse-tensor-type t nil t nil)
	      (denotation t nil t nil))

