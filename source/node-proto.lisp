
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


;; Subsequence visualize method calling, can access to initializer-map
(defparameter *initializer-map* nil)

(defmethod visualize ((proto Node-Proto))
  "              [ ]       [ ]
  |  []           |         |
  |------|        |         |
         |        |         |
  |-----------------------------------
  | Inputs:                          |
  |  ...                             |
  |-----------------------------------
  | Gemm (...)                       |
  | alpha=1.0                        |
  | beta=1.0 .......                 |
  |----------------------------------|
        |         |         |
      [XXX]     [XXX]     [XXX]
"
  (labels ((indenter (content n)
	     (with-output-to-string (out)
	       (format out "~a " content)
	       (dotimes (i (- n (length content))) (princ " " out))))
	   (maybe-find-tensor (key)
	     (let ((result
		     (when *initializer-map*
		       (or
			(get-initializer-map *initializer-map* key)
			(get-initializer-map *initializer-map* key t)))))
	       result)))
    
    (let* ((graph-name
	     ;; TODO: omit the longer name
	     (format nil "~a (~a) "
		     (node-proto-op-type proto)
		     (node-proto-name proto)))
	   (constants
	     (loop for proto in (node-proto-input proto)
		   for out = (maybe-find-tensor proto)
		   if out
		     collect (list (slot-value out 'name) (visualize (slot-value out 'dims)))))
	   (longest-name-constants
	     (apply #'max `(0 ,@(map 'list (alexandria:compose #'length #'car) constants))))
	   (attributes
	     (loop for attrs in (node-proto-attribute proto)
		   collect (list (attribute-proto-name attrs) (read-attr attrs))))
	   (longest-name-attrs
	     (apply #'max `(0 ,@(map 'list
				     (alexandria:compose #'length #'car) attributes))))
	   (things-to-display
	     `(,graph-name
	       ,@(map 'list #'(lambda (x) (format nil "~a = ~a" (first x) (second x))) attributes)
	       ,@(map 'list #'(lambda (x) (format nil "~a = ~a" (first x) (second x))) constants)))	       
	   (height (+ 4 (length things-to-display)))
	   (width  (+ 3 (apply #'max (map 'list #'length things-to-display)))))

      ;; No attrs, no constants
      (when (and (= 0 longest-name-constants)
		 (= 0 longest-name-attrs))
	(decf height 2))
      (with-output-to-string (stream)
	(cl-easel:with-easel (canvas (1 height width))
	  ;;(cl-easel:draw-horizontal! canvas 0 2 2)

	  ;; Draw the box
	  (cl-easel:draw-horizontal! canvas 0 0 width)
	  (cl-easel:draw-horizontal! canvas 2 0 width)
	  (cl-easel:draw-horizontal! canvas (1- height) 0 width)

	  (cl-easel:draw-vertical! canvas 0 0 height)
	  (cl-easel:draw-vertical! canvas (1- width) 0 height)

	  (let ((count 2))
	    (symbol-macrolet ((write-consts
				(progn
				  (dolist (const constants)
				    (incf count)
				    (let ((out (format nil "~a~a" (indenter (car const) longest-name-constants) (second const))))
				      (cl-easel:carve-text canvas out count 1)))
				  (incf count)
				  (cl-easel:draw-horizontal! canvas count 0 width)))
			      (write-attrs
				(progn
				  (dolist (attr attributes)
				    (incf count)
				    (let ((out (format nil "~a~a" (indenter (car attr) longest-name-attrs) (second attr))))
				      (cl-easel:carve-text canvas out count 1))))))
	      (when (not (= 0 longest-name-constants)) write-consts)
	      (when (not (= 0 longest-name-attrs)) write-attrs)))
	  

	  (cl-easel:carve-text canvas graph-name 1 1)
	  
	  (cl-easel:realize canvas)
	  (format stream "~%~a" canvas))))))

(cl-annot-revisit:export
  (defun viewnode (graph-proto)
    "Renders the computational graph"
    (declare (type Graph-Proto graph-proto))
    (let ((*initializer-map* (make-initializer-map graph-proto)))
      (format nil "~a" graph-proto))))
      
;; (defmethod connect-node (from node to))
