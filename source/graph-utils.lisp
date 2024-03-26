
(in-package :cl-onnx)

(named-readtables:in-readtable cl-annot-revisit:at-syntax-readtable)

@cl-annot-revisit:export-structure
(defstruct (Initializer-Map
	    (:constructor
		make-initializer-map (graph-proto)))
  "(get-initializer-map map) to access the initializer table."
  (initializer-map
   (let ((table (make-hash-table :test #'equal)))
     (dolist (tensor-proto (graph-proto-initializer graph-proto))
       (setf (gethash (tensor-proto-name tensor-proto) table) tensor-proto))
     table)
   :type hash-table)
  (sparse-initializer-map
   (let ((table (make-hash-table :test #'equal)))
     (dolist (tensor-proto (graph-proto-sparse-initializer graph-proto))
       (setf (gethash (tensor-proto-name tensor-proto) table) tensor-proto))
     table)
   :type hash-table))

@cl-annot-revisit:export
(defun get-initializer-map (initializer-map key &optional (sparse-p nil))
  "Returns the key in initializer-map. If not found, returns nil.
Set sparse-p=t to access sparse-initializer-map"
  (gethash key
	   (if sparse-p
	       (initializer-map-sparse-initializer-map initializer-map)
	       (initializer-map-initializer-map initializer-map))))

(defun (setf get-initializer-map) (value initializer-map key &optional (sparse-p nil))
  (setf (gethash key
		 (if sparse-p		     
		     (initializer-map-sparse-initializer-map initializer-map)
		     (initializer-map-initializer-map initializer-map)))
	value))

