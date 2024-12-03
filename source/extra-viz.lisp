(defpackage :cl-onnx/viz
  (:use :cl :cl-onnx))

(in-package :cl-onnx)
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
      (if (and (= 0 longest-name-constants)
		 (= 0 longest-name-attrs))
	  (decf height 2)
	  (when (or (= 0 longest-name-constants)
		    (= 0 longest-name-attrs))
	    (decf height)))
      
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
	    (:constructor make-cdn (name x y a b bx by
				    &key
				      (rounding #'round)
				    &aux
				      (x (funcall rounding x))
				      (y (funcall rounding y))
				      (a (funcall rounding a))
				      (b (funcall rounding b))
				      (bx (funcall rounding bx))
				      (by (funcall rounding (1+ by))))))
  "
 (x, y)  
  -------
  |     |
  ------- (x+a, x+b)
     ^ (bx, by) indicates here (from where the next node should start an arrow)
"
  ;; boxes
  (name name :type string)
  (x x :type fixnum)
  (y y :type fixnum)
  (a a :type fixnum)
  (b b :type fixnum)
  
  (bx bx :type fixnum) ;; branching point
  (by by :type fixnum)
  (seen nil))

(defun find-optimal-route (from-cdn to-cdn boxes stashed-list width)
  (declare (type coordinates from-cdn to-cdn)
	   (type list boxes))

  ;; stashed-arrows (list (x y_from y_to) ...)
  
  ;; Since the graph is DAG, it is asserted that:
  ;; - Side-by-side nodes are not dependent on each other.
  ;; - Dependent nodes are placed from top to bottom.
  
  ;; 1. attempts to align X
  ;; 2. attempts to align Y
  ;; 3. stash the line to the edge
  ;; Return -> a list of coodinates

  (labels ((try-horizontal ()
	     ;; Existing a, (to-cdn.bx + a) never conflicts with the area of (x + a) of each boxes -> try-horizontal
	     ;; For all x where from-cdn ~ to-cdn
	     (let* ((survived-area (range (c-bx to-cdn) (+ (c-a to-cdn) (c-x to-cdn)))))
	       (dolist (box boxes)
		 (when (and
			(intersection
			 (range (c-y box) (+ (c-y box) (c-b box)))
			 (range
			     (+ (c-b from-cdn) (c-y from-cdn))
			     (c-y to-cdn)))
			(not (string= (c-name box) (c-name to-cdn)))
			(not (string= (c-name box) (c-name from-cdn))))
		   ;; subject to search: Intersection(box, area) exists
		   (let* ((obstacle-range (range (c-x box) (+ (c-a box) (c-x box)))))
		     (mapc
		      #'(lambda (x)
			  (setf survived-area (remove x survived-area)))
		      obstacle-range))))
	       ;; the closer to the to-cdn.bx, the better
	       (let* ((mid (c-bx to-cdn))
		      (ranked (sort survived-area #'(lambda (x y) (< (abs (- mid x)) (abs (- mid y)))))))
		 (when ranked
		   (list :horizontal
			 (or
			  (and
			   (<= (abs (- (c-bx from-cdn) (c-bx to-cdn))) 1)
			   (find (c-bx to-cdn) ranked :test #'(lambda (x y) (<= (abs (- x y)) 1)))
			   (c-bx from-cdn))
			  (and
			   (find (c-bx to-cdn) ranked :test #'(lambda (x y) (<= (abs (- x y)) 1)))
			   (c-bx to-cdn))
			  ;; If align-to-the-nearst strategy failed,
			  ;; it is better to place the arrow in a distant location.
			  (nth 1 (reverse ranked))
			  (car (reverse ranked))))))))
	   (try-vertical ()
	     ;; Existing a, (to-cdn.bx + a) never conflicts with the area of (x + a) of each boxes -> try-horizontal
	     ;; For all x where from-cdn ~ to-cdn
	     (let* ((tgt-y-range (range (c-y to-cdn) (+ (c-y to-cdn) (c-b to-cdn)))))
	       (dolist (box boxes)
		 (when (and
			(intersection
			 (range (c-y box) (+ (c-y box) (c-b box)))
			 (range
			     (+ (c-b from-cdn) (c-y from-cdn))
			     (c-y to-cdn)))
			(not (string= (c-name box) (c-name to-cdn)))
			(not (string= (c-name box) (c-name from-cdn))))

		   ;; If conflicts in the x -> failed
		   (when (find (c-bx from-cdn) (range (c-x box) (+ (c-x box) (c-a box))))
		     (return-from try-vertical))
		   
		   ;; subject to search: Intersection(box, area) exists
		   (when (intersection
			  tgt-y-range
			  (range (c-y box) (+ (c-y box) (c-b box))))
		     (return-from try-vertical))))
	       (list :vertical (+ (c-y to-cdn) (floor (/ (c-b to-cdn) 2))))))	   
	   (try-complicated ()
	     ;; Renders the residual connection
	     ;; TODO: Improve the algorithm
	     (let ((go-left-p t)
		   (go-right-p t))
	       (dolist (box boxes)
		 (when (and
			(intersection
			 (range (c-y box) (+ (c-y box) (c-b box)))
			 (range
			     (+ (c-b from-cdn) (c-y from-cdn))
			     (c-y to-cdn)))
			(not (string= (c-name box) (c-name to-cdn)))
			(not (string= (c-name box) (c-name from-cdn))))

		   ;; if side-by-side with regard to from-cdn
		   (when (intersection
			  (range (c-y box) (+ (c-y box) (c-b box)))
			  (range (c-y from-cdn) (+ (c-y from-cdn) (c-b from-cdn))))

		     ;; to left
		     (when (intersection
			    (range 0 (c-x from-cdn))
			    (range (c-x box) (+ (c-x box) (c-a box))))
		       (setf go-left-p nil))

		     ;; to right
		     (when (intersection
			    (range (+ (c-x from-cdn) (c-a from-cdn)) width)
			    (range (c-x box) (+ (c-x box) (c-a box))))
		       (setf go-right-p nil)))))
	       
	       (when (and (null go-left-p) (null go-right-p))
		 (return-from try-complicated))

	       (let ((stashed-line-range
		       (if go-right-p
			   (range (+ (c-x from-cdn) (c-a from-cdn)) width)
			   (range 0 (c-x from-cdn)))))
		 
		 (dolist (box boxes)		   
		   (when (and
			  (intersection
			   (range (c-y box) (+ (c-y box) (c-b box)))
			   (range
			       (+ (c-b from-cdn) (c-y from-cdn))
			       (c-y to-cdn)))
			  (not (string= (c-name box) (c-name to-cdn)))
			  (not (string= (c-name box) (c-name from-cdn))))
		     (mapc
		      #'(lambda (x)
			  (setf stashed-line-range (remove x stashed-line-range)))
		      (range (c-x box) (+ (c-x box) (c-a box))))))

		 (loop for s in stashed-list
		       for x    = (nth 0 s)
		       for ymin = (nth 1 s)
		       for ymax = (nth 2 s)
		       if (intersection (range ymin ymax) (range (+ (c-b from-cdn) (c-y from-cdn)) (c-y to-cdn)))
			 do (setf stashed-line-range (remove x stashed-line-range)))

		 (setf stashed-line-range (sort stashed-line-range (if go-right-p #'> #'<)))

		 (when stashed-line-range
		   (let* ((median (floor (length stashed-line-range) 2))
			  (x (or (nth median stashed-line-range) (car stashed-line-range))))
		     (list :complicated
			   (list
			    (cons go-left-p go-right-p)
			    x
			    (list
			     x
			     (+ (c-b from-cdn) (c-y from-cdn))
			     (c-y to-cdn))))))))))		      
    
    (apply #'values
	   (or
	    (try-horizontal)
	    (try-vertical)
	    (try-complicated)))))

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

(defun find-the-window-width ()
  (handler-case
      (charms:with-curses () (charms:window-dimensions charms:*standard-window*))
    (t nil)))

(cl-annot-revisit:export
  (defun viewnode (graph-proto
		   &key
		     (width (or (find-the-window-width) 35))
		     (node-to-node-size 3)
		     (eager-subgraph-mode nil))
    "Implements Hierarchical drawing algorithm optimized for CUI, drawing the onnx graph into the REPL.
TODO:
    - display the shape of tensors
    - display the content of constant initializers"
    (declare (type Graph-Proto graph-proto)
	     (ignore eager-subgraph-mode))
    (with-indent (0)
      (let* ((*initializer-map* (make-initializer-map graph-proto))
	     (nodes  (map 'list #'visualize (graph-proto-node graph-proto)))
	     (inputs (map 'list (alexandria:compose #'write-in-box #'value-info-proto-name) (graph-proto-input graph-proto)))
	     (outputs (map 'list (alexandria:compose #'write-in-box #'value-info-proto-name) (graph-proto-output graph-proto)))
	     (name->position (make-hash-table :test #'equal))
	     ;; the tallest case for the height
	     (estimated-height
	       (+
		6 ;; the height of input + output
		(* 3 (length nodes))
		(apply #'+ (map 'list #'(lambda (x) (+ node-to-node-size (count #\Newline x))) nodes))))
	     ;; TODO: Fix this constant value (get a width?)
	     (estimated-width width)
	     (height-offset 0))
	(labels ((value->users (name)
		   (declare (type string name))
		   (loop for node in (graph-proto-node graph-proto)
			 for position upfrom 0
			 if (find name (node-proto-input node) :test #'equal)
			   collect (cons (nth position nodes) node)))
		 (user->values (name)
		   (declare (type string name))
		   (loop for node in (graph-proto-node graph-proto)
			 for position upfrom 0
			 if (find name (node-proto-output node) :test #'equal)
			   collect (cons (nth position nodes) node)))	   
		 (embody! (easel x y text &optional (n 2))
		   (multiple-value-bind (xp yp) (values 0 0)
		     (loop for line in (cl-ppcre:split (format nil "~%") text)
			   for yi upfrom y
			   do (cl-easel:carve-text easel line yi x)
			      (setf xp x yp yi))
		     (values (+ xp (floor (/ (width-of text) n))) yp)))
		 (read-constant (name)
		   ;; e.g.: name = ConstantXXX
		   (let ((users (user->values name)))
		     (when (= (length users) 1)
		       (and
			(equal "Constant" (node-proto-op-type (cdr (car users))))
			(cdr (car users)))))))
	  (with-output-to-string (out)
	    (cl-easel:with-easel (easel (1 estimated-height estimated-width))
	      ;; to understand the width of the windows
	      ;; (incf height-offset)
	      ;; (cl-easel:draw-horizontal! easel 0 0 estimated-width)
	      
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
		    for k = 0;;(/ (width-of input) 2)
		    do (incf offset xi)
		       (multiple-value-bind (bx by)
			   (embody! easel (floor (- offset k)) yi input)
			 (setf
			  (gethash (value-info-proto-name ip) name->position)
			  (make-cdn (value-info-proto-name ip) (floor offset) yi (width-of input) (length (cl-ppcre:split (format nil "~%") input)) bx by)))
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
		    (seen)
		    (stashed-arrows))
		(labels ((ready-p (name)
			   (or
			    (find name ready-nodes :test #'equal)
			    (get-initializer-map *initializer-map* name)
			    ;; ConstantNode arent displayed and recognised as ready
			    (read-constant name)))
			 (ready-to-render-p (node)
			   (let ((input (node-proto-input node)))
			     (node-proto-name node)
			     (every #'ready-p input)))
			 (connect-node (node user)
			   (declare (type string node user))
			   ;;          [ ] <- n_branch = length(value->users ...)
			   ;; h-off     |
			   ;; split ---------
			   ;;       |   |   |
			   ;;      p1   p2  p3 (coodinates, user)
			   ;;      n1   n2  n3 (next_p)
			   (let ((from (gethash node name->position))
				 (to   (gethash user name->position))
				 (k    (floor (/ node-to-node-size 2))))
			     (when (and from to (null (c-seen from)))
			       (cl-easel:draw-vertical! easel (c-bx from) (1- (c-by from)) (+ (c-by from) k 1))
			       (setf (c-seen from) t))
			     (when (and from to)
			       (multiple-value-bind (strategy points)
				   (find-optimal-route
				    from to (alexandria:hash-table-values name->position)
				    stashed-arrows estimated-width)
				 (case strategy
				   (:horizontal
				    (multiple-value-bind (a b |y1+y2|/2)
					(values
					 (min (c-bx from) points)
					 (max (c-bx from) points)
					 (floor (/ (- (c-y to) (+ (c-y from) (c-b from))) 2)))
				      (when (> (abs (- a b)) 1)
					(cl-easel:draw-horizontal!
					 easel
					 (+ |y1+y2|/2 (c-by from))
					 a
					 (1+ b))
					(when (> |y1+y2|/2 k)
					  (cl-easel:draw-vertical!
					   easel
					   (c-bx from)
					   (c-by from)
					   (+ |y1+y2|/2 (c-by from) k))))

				      (cl-easel:draw-vertical!
				       easel
				       points
				       (+ |y1+y2|/2 (c-by from))
				       (1+ (c-y to)))))				   
				   (:vertical
				    (cl-easel:draw-vertical!
				     easel
				     (c-bx from)
				     (c-by from)
				     (1+ points))
				    (multiple-value-bind (a b)
					(values
					 (c-bx from)					 
					 (if (<= (c-x from) (c-x to))
					     (c-x to)
					     (+ (c-x to) (c-a to))))
				      (cl-easel:draw-horizontal!
				       easel
				       points
				       (min a b)
				       (max a b))))				   
				   (:complicated
				    (multiple-value-bind (left-or-right stash-to info)
					(apply #'values points)

				      (push info stashed-arrows)
				      (let* ((start1 (if (cdr left-or-right)
							 (+ (c-x from) (c-a from))
							 (c-x from)))
					     (start2 (if (cdr left-or-right)
							 (+ (c-x to) (c-a to))
							 (c-x to)))
					     (from-mid (+ -1 (c-y from) (c-b from)))
					     (to-mid   (c-y to)))
					
					(multiple-value-bind (a1 b1 a2 b2)
					    (values
					     (min start1 stash-to)
					     (max start1 stash-to)
					     (min start2 stash-to)
					     (max start2 stash-to))
					  
					  (cl-easel:draw-horizontal!
					   easel
					   from-mid
					   a1
					   (1+ b1))

					  (cl-easel:draw-horizontal!
					   easel
					   to-mid
					   a2
					   (1+ b2))
					  
					  (cl-easel:draw-vertical!
					   easel
					   stash-to
					   from-mid
					   (1+ to-mid))))))			      
				   (t
				    (format t "Failed to connect: ~a -> ~a~%" node user)))))))
			 
			 (step-stashed-nodes (&aux (offset 0))
			   ;; Pop until offset reaches width
			   (let* ((not-ready)
				  (next-nodes)
				  (nodes (loop while (and (<= offset estimated-width) (not (null stashed-nodes)))
					       for tgt = (pop stashed-nodes)
					       for w = (+ 2.5 (width-of (car tgt))) ;; keep clearance of the edge.
					       for notseen-p = (not (find (node-proto-name (cdr tgt)) seen :test #'equal))
					       if (and
						   (ready-to-render-p (cdr tgt))
						   notseen-p
						   (<= (+ offset w) estimated-width))
						 collect
						 (progn
						   (push (node-proto-name (cdr tgt)) seen)
						   (incf offset w)
						   tgt)
					       else
						 do (when notseen-p (push tgt not-ready))))
				  (highest-height 0))

			     (setf stashed-nodes `(,@not-ready ,@stashed-nodes))

			     (when nodes
			       (let ((scale (/ estimated-width (+ (length nodes) (apply #'+ (map 'list (alexandria:compose #'width-of #'car) nodes))))))
				 ;; Bunch nodes
				 (incf height-offset node-to-node-size)

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
					  (multiple-value-bind (x y)
					      (embody! easel (round (- xoffset k)) yi str (1+ (length outputs)))
					    (loop with s = (/ (width-of str) (length outputs))
						  for out in outputs
						  for n upfrom 0
						  do (push out ready-nodes)
						     (setf (gethash out name->position)
							   (make-cdn
							    out
							    (round (- xoffset k))
							    yi
							    (width-of str)
							    (length (cl-ppcre:split (format nil "~%") str))
							    (+ x (* n s)) y)
							   next-nodes `(,@next-nodes ,@(value->users out))
							   (gethash (node-proto-name node) name->position)
							   (gethash out name->position))
						     (dolist (input (node-proto-input node))
						       ;; If the input is constnat, also needs to be displayed
						       (let ((const (read-constant input)))
							 (if const
							     nil;; nil TODO: Display the value of constant.
							     (connect-node input out))))))
					  (incf xoffset xi)
					  (setf highest-height
						(max highest-height (length (cl-ppcre:split (format nil "~%") str))))))
			       
			       (incf height-offset highest-height)
			       (setf stashed-nodes `(,@stashed-nodes ,@next-nodes))
			       (when stashed-nodes
				 (step-stashed-nodes))))))
		  ;; 1. add stashed-nodes
		  ;; 2. given width if the nodes cannot be displaed once stash them
		  ;; 3. display until stashed nodes are used
		  ;; 4. add new users
		  (step-stashed-nodes)

		  ;; compare unseen and nodes
		  ;; display: these nodes are not display because not topologically sorted or the width is too narrow.

		  (mapc
		   #'(lambda (x)
		       (when (null (find (node-proto-name x) seen))
			 (when (null (value->users (node-proto-name x)))
			   (warn "~a is isolated from the graph." (node-proto-name x)))))
		   (graph-proto-node graph-proto))

		  (incf height-offset node-to-node-size)

		  ;; Rendering the outputs
		  (loop with scale = (/ estimated-width (+ (length outputs) (apply #'+ (map 'list #'width-of outputs))))
			with offset = 0
			;; scale >= 1.0 assertion
			for output in outputs
			for ip in (graph-proto-output graph-proto)
			for xi = (/ (* scale (width-of output)) 2)
			for yi = height-offset
			for k = 1;;(/ (width-of output) 2)
			do (incf offset xi)
			   (multiple-value-bind (bx by)
			       (embody! easel (floor (- offset k)) yi output)
			     (setf
			      (gethash (value-info-proto-name ip) name->position)
			      (make-cdn (value-info-proto-name ip) (floor offset) yi (width-of output) (length (cl-ppcre:split (format nil "~%") output)) bx by)))
			   (dolist (from (user->values (value-info-proto-name ip)))
			     (connect-node (node-proto-name (cdr from)) (value-info-proto-name ip)))
			   (incf offset xi))))
	      (incf height-offset node-to-node-size)
	      (incf height-offset 3)
	      (cl-easel:realize easel)
	      (let ((result (cl-ppcre:split (format nil "~%") (format nil "~%~a" easel))))
		(if (<= (length result) height-offset)
		    (format out "~a~%" easel)
		    (dolist (line (subseq result 0 height-offset))
		      (format out "~a~%" line)))))))))))

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
	   (width (+ 2 (apply #'max (map 'list #'length things-to-display))))
	   (height (+ 1 (length things-to-display))))
      
      (with-output-to-string (stream)
	(cl-easel:with-easel (canvas (1 height width))
	  ;; Draws the box
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
