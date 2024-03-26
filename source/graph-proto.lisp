
(in-package :cl-onnx)

(define-proto (graph-proto cl-protobufs.onnx:graph-proto)
	      (node node-proto t t t)
	      (name string nil t nil)
	      (initializer tensor-proto t t t)
	      (sparse-initializer sparse-tensor-proto t t t)
	      (doc-string string nil t nil)
	      (input value-info-proto t t t)
	      (output value-info-proto t t t)
	      (value-info value-info-proto t t t)
	      (quantization-annotation tensor-annotation t t t)
	      (metadata-props string-string-entry-proto t t t))

(defmethod visualize ((proto Graph-Proto))
  "
  |- Gemm ---------------------------|
  | inputs                           |
  | outputs                          |
  |----------------------------------|
"

  (flet ((indenter (n)
	   #'(lambda (x)
	       (with-output-to-string (out)
		 (dotimes (i n) (princ " "out))
		 (format out "~a" x)))))
    
    (let* ((graph-name (graph-proto-name proto))
	   (inputs
	     (map
	      'list
	      (alexandria:compose
	       (indenter 2)
	       #'visualize)
	      (graph-proto-input proto)))
	   (outputs
	     (map
	      'list
	      (alexandria:compose (indenter 2) #'visualize)
	      (graph-proto-output proto)))
	   (things-to-display
	     `(,graph-name
	       "inputs :"
	       ,@inputs
	       "outputs :"
	       ,@outputs))
	   (width (+ 4 (apply #'max (map 'list #'length things-to-display))))
	   (height (+ 2 (length things-to-display))))
      
      (with-output-to-string (stream)
	(cl-easel:with-easel (canvas (1 height width))
	  ;;(cl-easel:draw-horizontal! canvas 0 2 2)

	  ;; Draw the box
	  (cl-easel:draw-horizontal! canvas 0           0 width)
	  (cl-easel:draw-horizontal! canvas (1- height) 0 width)

	  (cl-easel:draw-vertical! canvas 0 0 height)
	  (cl-easel:draw-vertical! canvas (1- width) 0 height)

	  (cl-easel:carve-text canvas (format nil "[~a]" graph-name) 0 2)

	  (loop for height upfrom 1
		for thing in (cdr things-to-display)
		do (cl-easel:carve-text canvas thing height 1))
	  
	  (cl-easel:realize canvas)
	  (format stream "~a" canvas))))))

