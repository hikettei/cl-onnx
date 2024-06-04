
(in-package :cl-onnx)

(defparameter *indent* 0)
(defparameter *indent-first-line* 0)

(defmacro with-indent ((indent-width &optional (indent-first-line indent-width)) &body body)
  `(let ((*indent* ,indent-width)
	 (*indent-first-line* ,indent-first-line))
     ,@body))

;; TODO: autogenerate print-object
(defparameter *visualize* t)
(defmethod visualize :around (proto)
  (let ((visualized (format nil "~a" (if *visualize* (call-next-method) (format nil "<~a>" (class-name (class-of proto)))))))
    (with-output-to-string (out)
      (loop with
	      indentation1 string = (apply
				     #'concatenate
				     'string
				     (map
				      'list
				      #'(lambda (x)
					  (declare (ignore x))
					  " ")
				      (range 0 *indent*)))
	    with
	      indentation2 string = (apply
				     #'concatenate
				     'string
				     (map
				      'list
				      #'(lambda (x)
					  (declare (ignore x))
					  " ")
				      (range 0 *indent-first-line*)))
	    with lines = (cl-ppcre:split (format nil "~%") visualized)
	    for line in lines
	    for n upfrom 1
	    do (format out "~a~a~a"
		       (if (= n 1)
			   indentation2
			   indentation1)
		       line
		       (if (= n (length lines))
			   ""
			   (format nil "~%")))))))

;; Failed Cases
(defmethod visualize ((proto null)) "nil")
(defmethod visualize ((proto string)) proto)
(defmethod visualize ((proto t))
  (format nil "~a" proto))

