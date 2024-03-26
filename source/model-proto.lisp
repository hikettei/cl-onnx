
;;
;; Provides a user-friendly wrapper for ModelProto
;;

(in-package :cl-onnx)

(define-proto (model-proto cl-protobufs.onnx:model-proto)
  ;; slot-name type protobuf-p optional? list?
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

;; (defmethod visualize ((proto Model-Proto) indent stream))

