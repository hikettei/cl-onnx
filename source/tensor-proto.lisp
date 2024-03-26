
(in-package :cl-onnx)

(define-proto (tensor-proto cl-protobufs.onnx:tensor-proto)
	      (dims fixnum nil t t)
	      (data-type fixnum nil t nil)
	      (segment keyword nil t nil)
	      (float-data t nil t nil)
	      (string-data t nil t nil)
	      (int64-data t nil t nil)
	      (name string nil t nil)
	      (doc-string string nil t nil)
	      (raw-data t nil t nil)
	      (external-data string-string-entry-proto t t t)
	      (data-location keyword nil t nil)
	      (double-data double-float nil t t)
	      (uint64-data (unsigned-byte 64) nil t t)
	      (metadata-props string-string-entry-proto t t t))

(cl-annot-revisit:export
  (defun int->data-type (int)
    "Converts the given integer into the keyword representing the dtype"
    (cl-protobufs.onnx:tensor-proto.data-type-int-to-keyword int)))

;; TODO: Implement Decoder

;; cl-pack
;; TODO: Optimize
;; TODO: Integer, BF16
;; self-implement pack/unpack

(defun type->lisp (proto)
  (case (cl-protobufs.onnx:tensor-proto.data-type-int-to-keyword (tensor-proto-data-type proto))
    (:float 'single-float)
    (T (error "not yet implemented (todo)"))))

(defun type->packprefix (proto)
  (case (type->lisp proto)
    ('single-float "f")))

(cl-annot-revisit:export
  (defmethod raw->array ((proto Tensor-Proto))
    ;; TODO: Optimize
    ;; Fix: Slow
    (let* ((raw-data (tensor-proto-raw-data proto))
	   (step (/ (array-total-size raw-data) (apply #'* (shape proto))))
	   (storage (make-array (apply #'* (shape proto)) :element-type (type->lisp proto))))
      (assert (typep step 'fixnum))
      (loop for i upfrom 0 below (array-total-size raw-data) by step
	    for index fixnum upfrom 0
	    for string = (with-output-to-string (out)
			   (loop for k upfrom i below (+ i step)
				 do (princ (code-char (aref (tensor-proto-raw-data proto) k)) out)))
	    do (setf (aref storage index) (cl-pack:unpack (type->packprefix proto) string)))
      (make-array (shape proto) :element-type (type->lisp proto) :displaced-to storage))))

;; (defmethod array->raw ((proto Tensor-Proto))  )
