
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
  :depends-on (:cl-protobufs
	       :cl-onnx/proto
	       :mgl-pax
	       :cl-annot-revisit
	       :cl-ppcre
	       :alexandria
	       :cl-easel
	       :cl-pack)
  :pathname "source"
  :components ((:file "package")
	       (:file "utils")
	       (:file "facet")
	       (:file "model-proto")
	       (:file "attribute-proto")
	       (:file "value-info-proto")
	       (:file "node-proto")
	       (:file "training-info-proto")
	       (:file "string-string-entry-proto")
	       (:file "tensor-annotation")
	       (:file "graph-proto")
	       (:file "tensor-proto")
	       (:file "sparse-tensor-proto")
	       (:file "tensor-shape-proto")
	       (:file "type-proto")
	       (:file "operator-set-id-proto")
	       (:file "function-proto")
	       (:file "visualize")
	       (:file "io")
	       (:file "doc")
	       (:file "nicknames")
	       (:file "graph-utils")))

(asdf:defsystem "cl-onnx/test"
  :author "hikettei"
  :licence "MIT"
  :description "Test suites for cl-onnx"
  :serial t
  :depends-on (:rove)
  :pathname "test"
  :components ((:file "package")))

