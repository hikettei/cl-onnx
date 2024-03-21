
(cl:defpackage :cl-onnx
  (:nicknames :onnx)
  (:use :cl)
  ;; Input/Output
  (:export
   #:load-model
   #:save-model
   ))

(cl:in-package :cl-onnx)

(mgl-pax:defsection @cl-onnx
    (:title "cl-onnx user guide"))
