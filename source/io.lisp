
(in-package :cl-onnx)

(declaim (ftype (function ((or pathname string)) Model-Proto) load-model))
(defun load-model (path)
  "load-model"
  (declare (type (or pathname string) path))
  (with-open-file (stream path :direction :input :element-type 'unsigned-byte)
    (let ((result
	    (protobuf->onnx
	     (cl-protobufs:deserialize-from-stream 'cl-protobufs.onnx:model-proto stream))))
      result)))

(declaim (ftype (function ((or pathname string) Model-Proto) boolean) save-model))
(defun save-model (path model-proto)
  "save-model"
  (declare (type (or pathname string) path)
	   (type model-proto model-proto))
  
  nil)
