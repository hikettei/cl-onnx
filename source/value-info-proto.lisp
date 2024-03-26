
(in-package :cl-onnx)

(define-proto (value-info-proto cl-protobufs.onnx:value-info-proto)
	      ;; name type onnxp optionalp listp
	      (name string nil t nil)
	      (type type-proto t t nil)
	      (doc-string string nil t nil)
	      (metadata-props string-entry-entry-proto t t t))

(defmethod visualize ((proto Value-Info-Proto))
  (format nil "~a~a"
	  (value-info-proto-name proto)
	  (visualize (value-info-proto-type proto))))

