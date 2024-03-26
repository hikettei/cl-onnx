
(in-package :cl-onnx)

(define-proto (node-proto cl-protobufs.onnx:node-proto)
	      ;; name type onnxp optionalp listp
	      (input string nil t t)
	      (output string nil t t)
	      (name string nil t nil)
	      (op-type string nil t nil)
	      (domain string nil t nil)
	      (overload string nil t nil)
	      (attribute attribute-proto t t t)
	      (doc-string string nil t nil)
	      (metadata-props string-string-entry-proto t t t))

(defmethod visualize ((proto Node-Proto))
  "
  |- Gemm ---------------------------|
  | inputs                           |
  | outputs                          |
  |----------------------------------|
"
  (let* ((opset-name (node-proto-type proto))
	 (inputs     (format nil "~a" (node-proto-input proto)))
	 (outputs    (format nil "~a" (node-proto-output proto)))
	 (attributes (map
		      'list
		      #'(lambda (attr)
			  "")
		      (graph-proto-attribute )
	 (things-to-display `(,opset-name ,inputs ,outputs ,@attributes)))
	 
    (cl-easel:with-easel (canvas (1 3 20))
      (cl-easel:realize canvas)
      canvas)))

