
(in-package :cl-onnx)

(define-proto (type-proto.tensor cl-protobufs.onnx:type-proto.tensor)
	      (elem-type fixnum nil t nil)
	      (shape tensor-shape-proto t t nil))

(defmethod visualize ((proto type-proto.tensor))
  (format nil "[~(~a~)]~a"
	  (int->data-type (type-proto.tensor-elem-type proto))
	  (visualize (type-proto.tensor-shape proto))))

(define-proto (type-proto.sequence cl-protobufs.onnx:type-proto.sequence)
	      (elem-type fixnum nil t nil))

(defmethod visualize ((proto type-proto.sequence))
  (format nil "Sequence[~a]" (int->data-type (type-proto.sequence-elem-type proto))))

(define-proto (type-proto.map cl-protobufs.onnx::type-proto.map)
	      (key-type fixnum nil t nil)
	      (value-type type-proto t t nil))

(defmethod visualize ((proto type-proto.map))
  (format nil "Map[~a, ~a]"
	  (int->data-type (type-proto.map-key-type proto))
	  (visualize (type-proto.map-value-type proto))))

(define-proto (type-proto.optional cl-protobufs.onnx:type-proto.optional)
	      (elem-type fixnum nil t nil))

(defmethod visualize ((proto type-proto.optional))
  (format nil "Optional[~a]" (int->data-type (type-proto.optional-elem-type proto))))

(define-proto (type-proto.sparse-tensor cl-protobufs.onnx:type-proto.sparse-tensor)
	      (elem-type fixnum nil t nil)
	      (shape tensor-shape-proto t t nil))

(defmethod visualize ((proto type-proto.sparse-tensor))
  (format nil "~a" (visualize (type-proto.sparse-tensor-shape proto))))

(define-proto (type-proto cl-protobufs.onnx:type-proto)
	      (value oneof t t nil)
	      (denotation t nil t nil))

(defmethod visualize ((proto Type-Proto))
  (visualize (type-proto-value proto)))

