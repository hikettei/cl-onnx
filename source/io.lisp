
(in-package :cl-onnx)


(defun load-onnx-model (path)
  (with-open-file (stream path :direction :input :element-type 'unsigned-byte)
    (cl-protobufs:deserialize-from-stream 'cl-protobufs.onnx:model-proto stream)))

