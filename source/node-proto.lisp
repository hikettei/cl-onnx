
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
	  (format stream "~a" canvas))))))

(defun write-in-box (text)
  (with-output-to-string (stream)
    (let ((width (length text)))
      (cl-easel:with-easel (easel (1 3 (+ 2 width)))
	(cl-easel:draw-horizontal! easel 0 0 (+ 2 width))
	(cl-easel:draw-horizontal! easel 2 0 (+ 2 width))
	(cl-easel:draw-vertical! easel 0 0 3)
	(cl-easel:draw-vertical! easel (+ 1 width) 0 3)
	(cl-easel:carve-text easel text 1 1)
	(cl-easel:realize easel)
	(format stream "~a" easel)))))


(defstruct (coordinates
	    (:conc-name c-)
	    (:constructor make-coordinates (x y)))
  "
  -------
  |     |
  -------
     ^ indicates here (from where the next node should start an arrow)
"
  (points `(,(cons x y)) :type list)
  (bx x) ;; branching point
  (by (1+ y)))

(cl-annot-revisit:export
  (defun viewnode (graph-proto &key (width 35))
    "Renders the computational graph"
    (declare (type Graph-Proto graph-proto))
    (with-indent (0)
      (let* ((*initializer-map* (make-initializer-map graph-proto))
	     (nodes  (map 'list #'visualize (graph-proto-node graph-proto)))
	     (inputs (map 'list (alexandria:compose #'write-in-box #'value-info-proto-name) (graph-proto-input graph-proto)))
	     (name->position (make-hash-table :test #'equal))
	     (estimated-height
	       (+
		2
		(* 3 (length nodes))
		(apply #'+ (map 'list #'(lambda (x) (+ 2 (count #\Newline x))) nodes))))
	     ;; TODO: Fix this constant value (get a width?)
	     (estimated-width width)
	     (height-offset 0))
	(labels ((value->users (name)
		   (declare (type string name))
		   (loop for node in (graph-proto-node graph-proto)
			 for position upfrom 0
			 if (find name (node-proto-input node) :test #'equal)
			   collect (cons (nth position nodes) node)))
		 (width-of (text)
		   (let ((lines (cl-ppcre:split (format nil "~%") text)))
		     (length (or (nth 1 lines) (nth 0 lines)))))
		 (embody! (easel x y text &optional (n 2))
		   (multiple-value-bind (xp yp) (values 0 0)
		     (loop for line in (cl-ppcre:split (format nil "~%") text)
			   for yi upfrom y
			   do (cl-easel:carve-text easel line yi x)
			      (setf xp x yp yi))
		     (values (+ xp (round (/ (width-of text) n))) yp))))
	  (with-output-to-string (out)
	    (cl-easel:with-easel (easel (1 estimated-height estimated-width))
	      ;; for debug
	      (incf height-offset)
	      (cl-easel:draw-horizontal! easel 0 0 estimated-width)
	      
	      ;; Draws the inputs
	      ;; estimated_width = scale * width1 + scale * width2 + scale * width3 ...
	      ;; estimated_width/scale = width1 + width2 + width3
	      ;; scale = estimated_width / (width1 + width2 + width3 ..)
	      (loop with scale = (/ estimated-width (+ (length inputs) (apply #'+ (map 'list #'width-of inputs))))
		    with offset = 0
		    ;; scale >= 1.0 assertion
		    for input in inputs
		    for ip    in (graph-proto-input graph-proto)
		    for xi = (/ (* scale (width-of input)) 2)
		    for yi = height-offset
		    for k = 0;(/ (width-of input) 2)
		    do (incf offset xi)
		       (setf
			(gethash (value-info-proto-name ip) name->position)
			(apply #'make-coordinates (multiple-value-list (embody! easel (round (- offset k)) yi input))))
		       (incf offset xi))
	      (incf height-offset 3)

	      (let ((ready-nodes
		      `(,@(map 'list #'value-info-proto-name (graph-proto-input graph-proto))
			,@(alexandria:hash-table-keys (initializer-map-initializer-map *initializer-map*))
			,@(alexandria:hash-table-keys (initializer-map-sparse-initializer-map *initializer-map*))))
		    (stashed-nodes
		      (loop for input in (graph-proto-input graph-proto)
			    for name = (value-info-proto-name input)
			    append (value->users name))))
		(labels ((ready-to-render-p (node)
			   (let ((input (node-proto-input node)))
			     (every #'(lambda (x) (find x ready-nodes :test #'equal)) input)))
			 (connect-node (node user)
			   (declare (type string node user))
			   ;;          [ ] <- n_branch = length(value->users ...)
			   ;; h-off     |
			   ;; split ---------
			   ;;       |   |   |
			   ;;      p1   p2  p3 (coodinates, user)
			   ;;      n1   n2  n3 (next_p)

			   ;; p1 p2 p3 h-offの次って仮定してない？
			   ;; 何手でいけるか
			   ;; next_p=FalseもStashする

			   (let ((cdn (gethash node name->position))
				 (usr (gethash user name->position)))
			     (when (and cdn usr)
			       (cl-easel:draw-vertical! easel (c-bx usr) (1- (c-by usr)) (1+ (c-by usr)))
			       )))
			 
			 (step-stashed-nodes (&aux (offset 0))
			   ;; Pop until offset reaches width
			   (let* ((not-ready)
				  (next-nodes)
				  (nodes (loop while (and (<= offset estimated-width) (not (null stashed-nodes)))
					       for tgt = (pop stashed-nodes)
					       for w = (+ 2.5 (width-of (car tgt))) ;; keep clearance of the edge.
					       if (and (ready-to-render-p (cdr tgt)) (<= (+ offset w) estimated-width))
						 collect
						 (progn
						   (incf offset w)
						   tgt)
					       else
						 do (push tgt not-ready)))
				  (highest-height 0))
			     (setf stashed-nodes `(,@not-ready ,@stashed-nodes))

			     (assert nodes () "Increase the number of width")
			     ;; Bunch nodes
			     (incf height-offset 2)

			     ;; Collected nodes are displayed in the line in the same way as inputs
			     (loop with scale = (/ estimated-width (+ (length nodes) (apply #'+ (map 'list (alexandria:compose #'width-of #'car) nodes))))
				   with xoffset = 0
				   for (str . node) in nodes
				   for xi = (/ (* scale (width-of str)) 2)
				   for yi = height-offset
				   for outputs = (node-proto-output node)
				   for k = (/ (width-of str) 2)
				   do (incf xoffset xi)
				      ;; TODO: connect the arrows here
				      (multiple-value-bind (x y)
					  (embody! easel (round (- xoffset k)) yi str (1+ (length outputs)))
					(loop with s = (/ (width-of str) (length outputs))
					      for out in outputs
					      for n upfrom 0
					      do (push out ready-nodes)
						 (setf (gethash out name->position) (make-coordinates (+ x (* n s)) y)
						       next-nodes `(,@next-nodes ,@(value->users out)))
						 (dolist (input (node-proto-input node))
						   (connect-node input out))))
				      (incf xoffset xi)
				      (setf highest-height
					    (max highest-height (length (cl-ppcre:split (format nil "~%") str)))))
			     
			     (incf height-offset highest-height)
			     (setf stashed-nodes `(,@stashed-nodes ,@next-nodes))
			     (when stashed-nodes
			       (step-stashed-nodes)))))
		  ;; 1. add stashed-nodes
		  ;; 2. given width if the nodes cannot be displaed once stash them
		  ;; 3. display until stashed nodes are used
		  ;; 4. add new users
		  (step-stashed-nodes)))
	      
	      (cl-easel:realize easel)
	      (format out "~a" easel))))))))
      
;; (defmethod connect-node (from node to))
