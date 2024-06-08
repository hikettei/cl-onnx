
(in-package :cl-onnx)

;; (name type onnx-p optionalp listp)
(define-proto (attribute-proto cl-protobufs.onnx:attribute-proto)
	      (name string nil t nil)
	      (ref-attr-name string nil t nil)
	      (doc-string string nil t nil)
	      (type keyword nil t nil)
	      (f single-float nil t nil)
	      (i fixnum nil t nil)
	      (s (vector (unsigned-byte 8)) nil t nil)
	      (g graph-proto t t nil)
	      (sparse-tensor sparse-tensor-proto t t nil)
	      ;; (cl-onnx-name onnx-name)
	      ((tp t) tensor-proto t t nil)
	      (floats list nil t t)
	      (ints list nil t t)
	      (strings list nil t t)
	      (tensors list t t t)
	      (graphs list t t t)
	      (type-protos list t t t))

(defmethod read-attr ((proto Attribute-Proto))
  (case (attribute-proto-type proto)
    (:TENSOR
     (if (tensor-proto-dims (attribute-proto-tp proto))
	 (raw->array (attribute-proto-tp proto))
	 (aref (raw->array (attribute-proto-tp proto)))))
    (:INTS
     (attribute-proto-ints proto))
    (:INT
     (attribute-proto-i proto))
    (:FLOAT
     (attribute-proto-f proto))
    (:STRING
     (attribute-proto-s proto))
    (T
     (warn "read-attr: not implemented: ~a" (attribute-proto-type proto))
     "?")))
