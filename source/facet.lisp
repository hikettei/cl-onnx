
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
			   (,@(loop for attrs in attributes
				    for attr-name1 = (nth 0 attrs)
				    for attr-name = (if (listp attr-name1) (car attr-name1) attr-name1)
				    collect attr-name))))
	     ,@(loop for attrs in attributes
		     for attr-name1 = (nth 0 attrs)
		     for type      = (nth 1 attrs)
		     for onnx-p    = (nth 2 attrs)
		     for optionalp = (nth 3 attrs)
		     for listp     = (nth 4 attrs)
		     for attr-name = (if (listp attr-name1) (car attr-name1) attr-name1)
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
		    for attr-name1 = (nth 0 attrs)
		    for type      = (nth 1 attrs)
		    for onnx-p    = (nth 2 attrs)
		    for optionalp = (nth 3 attrs)
		    for listp     = (nth 4 attrs)
		    for attr-name = (if (listp attr-name1) (car attr-name1) attr-name1)
		    for read-name = (if (listp attr-name1) (second attr-name1) attr-name1)
		    for reader = `(slot-value proto ',(make-reader read-name))
		    collect
		    (if listp
			`(map 'list #'(lambda (x) ,(if onnx-p `(protobuf->onnx x) 'x)) ,reader)
			(if onnx-p
			    `(protobuf->onnx ,reader)
			    reader)))))
	 
	 (defmethod onnx->protobuf ((onnx-object ,name))
	   (,protobuf-constructor
	    ,@(loop for attrs in attributes
		    for attr-name1 = (nth 0 attrs)
		    for onnx-p    = (nth 2 attrs)
		    for optionalp = (nth 3 attrs)
		    for listp     = (nth 4 attrs)
		    for attr-name = (if (listp attr-name1) (car attr-name1) attr-name1)
		    for key       = (intern (symbol-name (if (listp attr-name1) (second attr-name1) attr-name1)) "KEYWORD")
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
			      for attr-name1 = (nth 0 attrs) ;;
			      for type      = (nth 1 attrs) ;;
			      for onnx-p    = (nth 2 attrs) ;; onnx-p  -> Use (visualize ...) to render the object
			      for optionalp = (nth 3 attrs) ;; 
			      for listp     = (nth 4 attrs) ;; If List -> Visaluze the first element and omit subsequent elements
			      for attr-name = (if (listp attr-name1) (car attr-name1) attr-name1)
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
				   (labels ((reader (n)
					      (alexandria:compose
					       ;; If List, subsequent elements can also be displayed? or should be omitted?
					       (if ,listp
						   #'(lambda (result &aux (cnt (count (format nil "~%") result)))
						       (if (or
							    (>= cnt 1)
							    (null (nth (1+ n) (,(->accessor attr-name) onnx-object))))
							   (format nil "~a ~a" result (if (= cnt 0) "" "..."))
							   (format nil "~a ~a" result (funcall (reader (1+ n)) onnx-object))))
						   #'(lambda (x) x))
					       (if ,(eql type 'string)
						   #'(lambda (x) (format nil "\"~a\"" x))
						   #'(lambda (x) x))
					       (if ,onnx-p
						   #'visualize
						   #'(lambda (x) (format nil "~a" x)))
					       (if ,listp
						   #'(lambda (x)
						       (declare (type list x))
						       (nth n x))
						   #'(lambda (x) x))						 
					       #',(->accessor attr-name))))
				     (reader 0))
				   onnx-object))
			       (format nil "~%"))))))))))))

;; TODO: Identify is it really required or not,
(defmethod protobuf->onnx ((proto null)) proto)
(macrolet ((def (name)
	     `(progn
		(defmethod protobuf->onnx ((proto ,name)) proto)
		(defmethod onnx->protobuf ((proto ,name)) proto))))
  (def string)
  (def fixnum))

(cl-annot-revisit:export-structure
  (defstruct OneOf
    value
    set-field))

(defmethod protobuf->onnx ((proto cl-protobufs.implementation::oneof))
  (make-oneof
   :value
   (protobuf->onnx
    (cl-protobufs.implementation::oneof-value proto))
   :set-field
   (cl-protobufs.implementation::oneof-set-field proto)))

(defmethod onnx->protobuf ((proto OneOf))
  (cl-protobufs.implementation::make-oneof
   :value (onnx->protobuf proto)
   :set-field (oneof-set-field proto)))

(defmethod print-object ((proto OneOf) stream)
  (format stream "#S(OneOf[set-field=~a] ~a)" (oneof-set-field proto) (oneof-value proto)))

(defmethod visualize ((proto OneOf))
  (visualize (oneof-value proto)))

;; (mgl-pax:defsection @facets (:title "Facets"))


