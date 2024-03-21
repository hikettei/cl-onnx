
CL            := ros
QUICKLOAD     := --load cl-onnx.asd --eval '(progn (load "cl-onnx.asd") (ql:quickload :cl-onnx))'
PYTHON := python

.DEFAULT_GOAL := help

.PHONY: help
help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'

.PHONY: export_dummy_graph
export_dummy_graph: ## First creates a dummy onnx graph to perform test harness.
	$(PYTHON) ./utils/export_dummy_graph.py


.PHONY: test
test: export_dummy_graph ## Executes the test harness.
	$(CL) $(QUICKLOAD) --eval '(asdf:test-system :cl-onnx)'

