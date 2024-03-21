
(asdf:defsystem "cl-onnx"
  :author "hikettei"
  :licence "MIT"
  :description "A Tiny ONNX Graph Manipulator for Common Lisp"
  :serial t
  :depends-on (:protobuf)
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

