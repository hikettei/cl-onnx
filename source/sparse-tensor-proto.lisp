
(in-package :cl-onnx)


(define-proto (sparse-tensor-proto cl-protobufs.onnx:sparse-tensor-proto)
	      (values tensor-proto t t nil)
	      (indices tensor-proto t t nil)
	      (dims fixnum nil t t))

