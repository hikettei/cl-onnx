
;;
;; Provides a user-friendly wrapper for ModelProto
;;

(in-package :cl-onnx)

;; name type-in-llisp onnx-protobuf-p optional-p listp
(define-proto (model-proto cl-protobufs.onnx:model-proto)
  (ir-version fixnum nil t nil)
  (opset-import operator-set-id-proto t nil t)
  (producer-name string nil t nil)
  (producer-version string nil t nil)
  (domain string nil t nil)
  (model-version fixnum nil t nil)
  (doc-string string nil t nil)
  (graph graph-proto t t nil)
  (metadata-props string-string-entry-proto t t t)
  (training-info training-info-proto t t t)
  (functions function-proto t t t))


;(defmethod print-object ((proto Model-Proto) stream)
;  (format stream "<Model-Proto~%ir_version=~a~a~a"
;	  (model-proto-ir-version proto)
;	  (model-proto-opset-import proto)
;	  (model-proto-producer-name proto)))

