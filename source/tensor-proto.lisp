
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
    (declare (optimize (speed 3)))
    ;; TODO: complex-numbers
    (when (= 0 (array-total-size (tensor-proto-raw-data proto)))
        (return-from raw->array nil))
    (let* ((raw-data (tensor-proto-raw-data proto))
	   (to   (array-total-size raw-data))
	   (step (/ to (the fixnum (apply #'* (the list (shape proto))))))
	   (dtype (int->data-type (tensor-proto-data-type proto)))
	   (window `(,step))
	   (elem-type (ecase dtype
			(:double 'double-float)
			(:float 'single-float)
			(:uint64 '(unsigned-byte 64))
			(:int64 '(signed-byte 64))
			(:uint32 '(unsigned-byte 32))
			(:int32 '(signed-byte 32))
			(:uint16 '(unsigned-byte 16))
			(:int16 '(signed-byte 16))
			(:uint8 '(unsigned-byte 8))
			(:int8 '(signed-byte 8))
			(:uint4 '(unsigned-byte 4))
			(:int4 '(signed-byte 4))))
	   (storage (make-array (apply #'* (shape proto)) :element-type elem-type))
	   (in-buffer (make-array window :element-type (array-element-type raw-data) :displaced-to raw-data :displaced-index-offset 0)))
      (declare (type (simple-array (unsigned-byte 8) (*)) raw-data))
      (assert (typep step 'fixnum))
      (flet ((displace (n)
	       (setf in-buffer (adjust-array in-buffer window :displaced-to raw-data :displaced-index-offset n)))	     
	     (read-float32 ()
	       (ieee-floats:decode-float32 (intbytes:octets->uint32 in-buffer)))
	     (read-float64 ()
	       (ieee-floats:decode-float64 (intbytes:octets->uint64 in-buffer)))
	     (read-int ()
	       (intbytes:octets->int in-buffer step)))
	(declare (inline read-float32 read-float64))
	(case dtype
	  (:double
	   (locally (declare (type (simple-array single-float (*)) storage))
	     (loop for i fixnum upfrom 0 below to by step
		   for n fixnum upfrom 0
		   do (displace i) (setf (aref storage n) (the single-float (read-float32))))))
	  (:float
	   (locally (declare (type (simple-array single-float (*)) storage))
	     (loop for i fixnum upfrom 0 below to by step
		   for n fixnum upfrom 0
		   do (displace i) (setf (aref storage n) (the single-float (read-float32))))))
	  (T
	   (progn;;locally (declare (type (simple-array t (*)) storage))
	     (loop for i fixnum upfrom 0 below to by step
		   for n fixnum upfrom 0
		   do (displace i) (setf (aref storage n) (read-int))))))
	(make-array (shape proto) :element-type elem-type :displaced-to storage)))))

;; (defmethod array->raw ((proto Tensor-Proto))  )
