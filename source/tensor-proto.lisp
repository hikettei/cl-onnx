
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

;; TODO: Implement Decoder
