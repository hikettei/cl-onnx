
CL            := ros
QUICKLOAD     := --load cl-onnx.asd --eval '(progn (load "cl-onnx.asd") (ql:quickload :cl-onnx))'
PROTOC := protoc
PYTHON := python

WORKDIR := $(shell dirname `which protoc`)
INSTALL_DIR := /usr/local/bin

.DEFAULT_GOAL := help

.PHONY: help
help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'

.PHONY: export_dummy_graph
export_dummy_graph: ## First creates a dummy onnx graph to perform test harness.
	$(PYTHON) ./utils/export_dummy_graph.py

.PHONY: install_deps
install_deps: ## Installs Quicklisp deps
	qlot install

.PHONY: install_cl_protobufs
install_cl_protobufs:
	cd cl-protobufs/protoc && PROTOC_ROOT=$(WORKDIR)/../ make && sudo cp protoc-gen-cl-pb $(INSTALL_DIR)

.PHONY: build
build: install_deps install_cl_protobufs ## Builds onnx.proto
	$(PROTOC) --plugin=protoc-gen-cl-pb=/usr/local/bin/protoc-gen-cl-pb --proto_path=./onnx/onnx --lisp_out=./source ./onnx/onnx/onnx.proto

.PHONY: test
test: build export_dummy_graph ## Executes the test harness.
	$(CL) $(QUICKLOAD) --eval '(asdf:test-system :cl-onnx)'

