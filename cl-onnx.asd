
(asdf:defsystem "cl-onnx/proto"
  :author "hikettei"
  :licence "MIT"
  :description "Automatically generated wrapper for onnx.proto"
  :serial t
  :depends-on (:cl-protobufs)
  :pathname "source"
  :components ((:file "onnx.proto")))

(asdf:defsystem "cl-onnx"
  :author "hikettei"
  :licence "MIT"
  :description "A Tiny ONNX Graph Manipulator for Common Lisp"
  :serial t
  :depends-on (:cl-protobufs :cl-onnx/proto :mgl-pax)
  :pathname "source"
  :components ((:file "package")))

(asdf:defsystem "cl-onnx/test"
  :author "hikettei"
  :licence "MIT"
  :description "Test suites for cl-onnx"
  :serial t
  :depends-on (:rove)
  :pathname "test"
  :components ((:file "package")))

