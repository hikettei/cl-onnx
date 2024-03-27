
# cl-onnx

CLI-Complete ONNX Binding and graph manipulator for Common Lisp.

## Workload

- [x] Protobuf <-> Common Lisp

- [ ] Implement well-displayed graph (as if CLI netron!)

- [ ] Find Opset Documentation on REPL

- [ ] C2FFI Based Binding for ONNX

- [ ] make maintainable

- [ ] utils (and useful macros) for accessing onnx graph in common lisp

- [ ] (onnx:view ...) -> window

## Background

- Emacs + SLIME Complete Developments since I am too lazy to run netron (esp when the graph is enough small.)

## Disclaimer

- Never use it for products as we cannot guarantee its quality.
    
- It is packed with utilities and functions used for personal development, so it is less tolerant of onnx updates.

- The renderer could not use ncurses to complete in Emacs. I gonna use my own engine.

## Requirements (TODO)

- protobuf

- qlot

- confirmed working on Linux/macOS


## Installing (TODO)

Follow these steps:

- install protobuf

- update gitsubmodule

- working on linux or macOS

```sh
$ make build
```
