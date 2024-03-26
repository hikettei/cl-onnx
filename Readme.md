
# cl-onnx

Common Lisp library for comfortably handling ONNX graphs on REPL.

## Workload

- [x] Protobuf <-> Common Lisp

- [ ] Implement well-displayed graph (as if CLI netron!)

- [ ] Find Opset Documentation on REPL

- [ ] C2FFI Based Binding for ONNX

- [ ] make maintainable

## Requirements (TODO)

- protobuf

- qlot

- confirmed working on Linux/macOS

## Memo

- RendererとかはProtoを読んであとはマクロで自動生成してるから，ONNX専用にする必要もない気がする

## Installing (TODO)

Follow these steps:

- install protobuf

- update gitsubmodule

- working on linux or macOS

```sh
$ make build
```
