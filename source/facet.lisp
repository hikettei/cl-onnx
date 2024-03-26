
(in-package :cl-onnx)

(defgeneric protobuf->onnx (proto)
  (:documentation "Protobuf Object -> cl-onnx structure"))

(defgeneric onnx->protobuf (onnx-object)
  (:documentation "cl-onnx structure -> protobuf object"))

(defmacro define-proto ((name protobuf-name) &rest attributes)
  "attributes = (name type-in-lisp onnx-protobuf-obj-p optional-p listp)"

  (assert (every #'(lambda (x) (= (length x) 5)) attributes)
	  ()
	  "Assertion Failed with each attributes has 5 elements: ~a" attributes)
  
  (let ((constructor (symb 'make- name))
	(protobuf-constructor
	  (let ((*package* (find-package :cl-protobufs.onnx)))
	    (symb 'make- name))))
    (flet ((make-reader (symbol &aux (*package* (find-package :cl-protobufs.onnx)))
	     (symb '% symbol))
	   (->accessor (symbol)
	     (symb name '- symbol)))
      `(progn
	 (cl-annot-revisit:export-structure
	   (defstruct (,name
		       (:constructor
			   ,constructor
			   ,(loop for attrs in attributes
				  for attr-name = (nth 0 attrs)
				  collect attr-name)))
	     ,@(loop for attrs in attributes
		     for attr-name = (nth 0 attrs)
		     for type      = (nth 1 attrs)
		     for onnx-p    = (nth 2 attrs)
		     for optionalp = (nth 3 attrs)
		     for listp     = (nth 4 attrs)
		     collect
		     `(,attr-name
		       ,(if listp
			    `(map 'list #'(lambda (x) ,(if onnx-p `(protobuf->onnx x) 'x)) ,attr-name)
			    (if onnx-p
				`(protobuf->onnx ,attr-name)
				attr-name))			
		       :type
		       ,(if optionalp
			    `(or
			      null
			      ,(if listp 'list type))
			    (if listp 'list type))))))
	 
	 
	 (defmethod protobuf->onnx ((proto ,protobuf-name))
	   (,constructor
	    ,@(loop for attrs in attributes
		    for attr-name = (nth 0 attrs)
		    for type      = (nth 1 attrs)
		    for onnx-p    = (nth 2 attrs)
		    for optionalp = (nth 3 attrs)
		    for listp     = (nth 4 attrs)
		    for reader = `(slot-value proto ',(make-reader attr-name))
		    collect
		    (if listp
			`(map 'list #'(lambda (x) ,(if onnx-p `(protobuf->onnx x) 'x)) ,reader)
			(if onnx-p
			    `(protobuf->onnx ,reader)
			    reader)))))
	 
	 (defmethod onnx->protobuf ((onnx-object ,name))
	   (,protobuf-constructor
	    ,@(loop for attrs in attributes
		    for attr-name = (nth 0 attrs)
		    for onnx-p    = (nth 2 attrs)
		    for optionalp = (nth 3 attrs)
		    for listp     = (nth 4 attrs)
		    for key       = (intern (symbol-name attr-name) "KEYWORD")
		    append
		    (list
		     key
		     `(let ((obj (slot-value onnx-object ',attr-name)))
			(if obj
			    ,(if listp
				 (if onnx-p
				     `(map 'list #'onnx->protobuf obj)
				     `(onnx->protobuf obj))
				 (if onnx-p
				     `(onnx->protobuf obj)
				     'obj))
			    :%unset))))))

	 ;; Chore for print-object
	 (defmethod print-object ((onnx-object ,name) stream)
	   (format stream "#S(~a~%~a)"
		   ',name
		   (apply
		    #'concatenate
		    'string
		    (butlast
		     (list
		      ,@(loop with prefixes = (map
					       'list
					       #'(lambda (x)
						   (format nil ":~(~a~) " (car x)))
					       attributes)
			      with indent-to = (apply #'max (map 'list #'length prefixes))
			      for attrs in attributes
			      for prefix in prefixes

			      for offset    = (- indent-to (length prefix))
			      for attr-name = (nth 0 attrs) ;;
			      for type      = (nth 1 attrs) ;;
			      for onnx-p    = (nth 2 attrs) ;; onnx-p  -> Use (visualize ...) to render the object
			      for optionalp = (nth 3 attrs) ;; 
			      for listp     = (nth 4 attrs) ;; If List -> Visaluze the first element and omit subsequent elements
			      append
			      (list
			       "    "
			       prefix
			       (apply
				#'concatenate
				'string
				(loop for i in (range 0 offset)
				      collect " "))
			       `(with-indent (,(+ indent-to 4) 0)
				  (funcall
				   (alexandria:compose
				    (if ,(eql type 'string)
					#'(lambda (x) (format nil "\"~a\"" x))
					#'(lambda (x) x))
				    (if ,onnx-p
					#'visualize
					#'(lambda (x) (format nil "~a" x)))
				    (if ,listp
					#'(lambda (x)
					    (declare (type list x))
					    (car x))
					#'(lambda (x) x))				    
				    #',(->accessor attr-name))
				   onnx-object))
			       (format nil "~%"))))))))))))
			  
		    
;; TODO: Identify is it really required or not,
(defmethod protobuf->onnx ((proto null))
  proto)

;;todo
(defmethod protobuf->onnx ((proto cl-protobufs.imeplementation::oneof))
  )

;; (mgl-pax:defsection @facets (:title "Facets"))


