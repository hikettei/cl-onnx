
(in-package :cl-onnx)

(defgeneric protobuf->onnx (proto)
  (:documentation "Protobuf Object -> cl-onnx structure"))

(defgeneric onnx->protobuf (onnx-object)
  (:documentation "cl-onnx structure -> protobuf object"))

(defmacro define-proto ((name protobuf-name) &rest attributes)
  "attributes = (name type-in-lisp onnx-protobuf-obj-p optional-p listp)"
  (let ((constructor (symb 'make- name))
	(protobuf-constructor
	  (let ((*package* (find-package :cl-protobufs.onnx)))
	    (symb 'make- name))))
    (flet ((make-reader (symbol &aux (*package* (find-package :cl-protobufs.onnx)))
	     (symb '% symbol)))
      `(progn
	 (named-readtables:in-readtable cl-annot-revisit:at-syntax-readtable)
	 @export
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
			    nil
			    ,(if listp 'list type))
			  (if listp 'list type)))))
	 
	 
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
	   :%unset
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
			    :%unset))))))))))

;; (mgl-pax:defsection @facets (:title "Facets"))

