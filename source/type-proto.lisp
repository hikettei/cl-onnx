
(in-package :cl-onnx)

(define-proto (type-proto cl-protobufs.onnx:type-proto)
	      (value cl-protobufs.implementation::oneof nil t nil)
	      (denotation t nil t nil))

(defmacro range (from to &optional (by 1))
  `(loop for i upfrom ,from below ,to by ,by collect i))

