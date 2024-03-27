
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
	    (:constructor make-coordinates (x y a b bx by)))
  "
 (x, y)  
  -------
  |     |
  ------- (x+a, x+b)
     ^ (bx, by) indicates here (from where the next node should start an arrow)
"
  ;; boxes
  (x x)
  (y y)
  (a a)
  (b b)
  
  (bx bx) ;; branching point
  (by (1+ by))
  (seen nil))

(defun all-permutations (list)
  (cond ((null list) nil)
        ((null (cdr list)) (list list))
        (t (loop for element in list
		 append (mapcar (lambda (l) (cons element l))
				(all-permutations (remove element list)))))))

(defun width-of (text)
  (let ((lines (cl-ppcre:split (format nil "~%") text)))
    (length (or (nth 1 lines) (nth 0 lines)))))

(defun minimize-distances (nodes name->positions scale)
  (declare (type list nodes)
	   (type hash-table name->positions))
  (when (= (length nodes) 1) (return-from minimize-distances nodes))

  (let ((best)
	(best-score))
    (labels ((distance (from x)
	       (declare (type string from))
	       (let ((cdn (gethash from name->positions)))
		 (if cdn
		     ;; the diff of x
		     (expt (- (c-bx cdn) x) 2)
		     0.0)))
	     (compute (pattern)
	       (let ((Σ 0.0)
		     (offset 0))
		 (loop for (str . node) in pattern
		       for xi = (/ (* scale (width-of str)) 2)
		       do (incf offset xi)
			  (dolist (in (node-proto-input node))
			    (incf Σ (distance in offset)))
			  (incf offset xi))
		 (when (or (null best-score)
			   (>= best-score Σ))
		   (setf best-score Σ
			 best pattern)))))
      (mapc #'compute (all-permutations nodes)))
    best))

;; Canvas (30 x 100)
;; TODO: Eager to know the width of the window
;; If failed, set manually
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
		 (embody! (easel x y text &optional (n 2))
		   (multiple-value-bind (xp yp) (values 0 0)
		     (loop for line in (cl-ppcre:split (format nil "~%") text)
			   for yi upfrom y
			   do (cl-easel:carve-text easel line yi x)
			      (setf xp x yp yi))
		     (values (+ xp (floor (/ (width-of text) n))) yp))))
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
		       (multiple-value-bind (bx by)
			   (embody! easel (floor (- offset k)) yi input)
		       (setf
			(gethash (value-info-proto-name ip) name->position)
			(make-coordinates xi yi (width-of input) (length (cl-ppcre:split (format nil "~%") input)) bx by)))
		       (incf offset xi))
	      (incf height-offset 3)

	      (let ((ready-nodes
		      `(,@(map 'list #'value-info-proto-name (graph-proto-input graph-proto))
			,@(alexandria:hash-table-keys (initializer-map-initializer-map *initializer-map*))
			,@(alexandria:hash-table-keys (initializer-map-sparse-initializer-map *initializer-map*))))
		    (stashed-nodes
		      (loop for input in (graph-proto-input graph-proto)
			    for name = (value-info-proto-name input)
			    append (value->users name)))
		    (seen))
		(labels ((ready-p (name)
			   (find name ready-nodes :test #'equal))
			 (ready-to-render-p (node)
			   (let ((input (node-proto-input node)))
			     (every #'ready-p input)))
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
			     (when (and cdn usr (null (c-seen cdn)))
			       (format t "~a -> ~a~%" node user)
			       (cl-easel:draw-vertical! easel (c-bx cdn) (- (c-by cdn) 0) (+ 2 (c-by cdn)))
			       (setf (c-seen cdn) t)

			       ;; 
			       )))
			 
			 (step-stashed-nodes (&optional (failed nil) &aux (offset 0))
			   ;; Pop until offset reaches width
			   (let* ((not-ready)
				  (next-nodes)
				  (nodes (loop while (and (<= offset estimated-width) (not (null stashed-nodes)))
					       for tgt = (pop stashed-nodes)
					       for w = (+ 2.5 (width-of (car tgt))) ;; keep clearance of the edge.
					       if (and
						   (ready-to-render-p (cdr tgt))
						   (not (find (node-proto-name (cdr tgt)) seen :test #'equal))
						   (<= (+ offset w) estimated-width))
						 collect
						 (progn
						   (push (node-proto-name (cdr tgt)) seen)
						   (incf offset w)
						   tgt)
					       else
						 do (push tgt not-ready)))
				  (highest-height 0))
			     (setf stashed-nodes `(,@not-ready ,@stashed-nodes))

			     (when nodes
			       (let ((scale (/ estimated-width (+ (length nodes) (apply #'+ (map 'list (alexandria:compose #'width-of #'car) nodes))))))
				 ;; Bunch nodes
				 (incf height-offset 2)

				 ;; weighten
				 (setf nodes (minimize-distances nodes name->position scale))

				 ;; Collected nodes are displayed in the line in the same way as inputs
				 (loop with xoffset = 0
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
						     (setf (gethash out name->position)
							   (make-coordinates
							    xi yi
							    (width-of str)
							    (length (cl-ppcre:split (format nil "~%") str))
							    (+ x (* n s)) y)
							   next-nodes `(,@next-nodes ,@(value->users out)))
						     (dolist (input (node-proto-input node))
						       (connect-node input out))))
					  (incf xoffset xi)
					  (setf highest-height
						(max highest-height (length (cl-ppcre:split (format nil "~%") str))))))
			       
			       (incf height-offset highest-height)
			       (setf stashed-nodes `(,@stashed-nodes ,@next-nodes))
			       (when stashed-nodes
				 (step-stashed-nodes)))

			     (when (and stashed-nodes (not failed))
			       (step-stashed-nodes t))
			     )))
		  ;; 1. add stashed-nodes
		  ;; 2. given width if the nodes cannot be displaed once stash them
		  ;; 3. display until stashed nodes are used
		  ;; 4. add new users
		  (step-stashed-nodes)

		  ;; compare unseen and nodes
		  ;; display: these nodes are not display because not topologically sorted or the width is too narrow.
		  ))
	      
	      (cl-easel:realize easel)
	      (format out "~a" easel))))))))

;; (defmethod connect-node (from node to))
