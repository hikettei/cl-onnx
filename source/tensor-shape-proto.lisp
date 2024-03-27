
(in-package :cl-onnx)

(define-proto (tensor-shape-proto.dimension cl-protobufs.onnx:tensor-shape-proto.dimension)
	      (value oneof t t nil))

(defmethod visualize ((proto tensor-shape-proto.dimension))
  (format nil "~a" (oneof-value (tensor-shape-proto.dimension-value proto))))

(define-proto (tensor-shape-proto cl-protobufs.onnx:tensor-shape-proto)
	      (dim tensor-shape-proto.dimension t t t)) ;; tensor-shape-proto.dimension

(defmethod visualize ((proto Tensor-Shape-Proto))
  (format nil "[~a]"
	  (apply
	   #'concatenate
	   'string
	   (butlast
	    (loop for dim in (tensor-shape-proto-dim proto)
		  append
		  (list
		   (visualize dim)
		   ", "))))))

