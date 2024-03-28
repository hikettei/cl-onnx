
# cl-onnx

CLI-Complete ONNX Graph Manipulator for Common Lisp (and more!).

## Disclaimer

- cl-onnx is in the early stage of development, has not been fully tested, and is unstable.

- I do not intend to make this a stable product because I am not confident that I will continue to update the accompanying useful utilities and other features.

    - If you are going to use cl-onnx just for reading .onnx graph, it is safer for your product to use [cl-protobufs](https://github.com/qitab/cl-protobufs)

    - And [here](https://github.com/hikettei/cl-onnx/blob/main/source/tensor-proto.lisp#L41) you can find how to unpack `raw_data` existing in TensorProto.

- Never use it for products as we cannot guarantee its quality.

- I think cl-onnx is dedicated to my personal projects.

## Workload

- [x] Protobuf <-> Common Lisp Objects

- [x] Implement DAG Visualizer (like CLI Netron), which is available on Emacs/REPL (i.e.: ncurses is unavailable.).

- [ ] Find Opset Documentation on REPL

- [ ] C2FFI Based Binding for ONNX

- [ ] utils (and useful macros) for accessing onnx graph in common lisp


# Features

## CLI Netron

I am too lazy to open netron to check the small graphs :< since we are already using a wonderful tool called REPL.

So I made a tool to draw the DAG in the REPL. ncurses is not used, so it should help you with fast deploying even in Emacs/Lem.

```
CL-USER> (onnx:viewnode (onnx:graph (onnx:load-model "./dummy_graph/layernorm.onnx")))
                       ┌─────┐                       
                       │input│                       
                       └──┬──┘────────────────┐      
                          │                   │      
            ┌─────────────┴────────────┐      │      
            │ReduceMean (/ReduceMean)  │      │      
            ├──────────────────────────┤      │      
            │axes     ?                │      │      
            │keepdims 1                │      │      
            └─────────────┬────────────┘      │      
                          │                   │      
                  ┌───────┴────┐──────────────┘      
                  │Sub (/Sub)  │                     
                  └──────┬─────┘──────────────┐      
                         │                    │      
                  ┌──────┴─────┐              │      
                  │Pow (/Pow)  │              │      
                  └──────┬─────┘              │      
                         │                    │      
           ┌─────────────┴──────────────┐     │      
           │ReduceMean (/ReduceMean_1)  │     │      
           ├────────────────────────────┤     │      
           │axes     ?                  │     │      
           │keepdims 1                  │     │      
           └──────────────┬─────────────┘     │      
                          │                   │      
                  ┌───────┴────┐              │      
                  │Add (/Add)  │              │      
                  └──────┬─────┘              │      
                         │                    │      
                 ┌───────┴──────┐             │      
                 │Sqrt (/Sqrt)  │             │      
                 └───────┬──────┘             │      
                         │                    │      
                  ┌──────┴─────┐──────────────┘      
                  │Div (/Div)  │                     
                  └──────┬─────┘                     
                         │                           
                 ┌───────┴─────┐                     
                 │Mul (/Mul)   │                     
                 ├─────────────┤                     
                 │gamma (10)   │                     
                 └──────┬──────┘                    
                        │                            
                 ┌──────┴───────┐                    
                 │Add (/Add_1)  │                    
                 ├──────────────┤                    
                 │beta (10)     │                    
                 └───────┬──────┘                    
                         │                           
                      ┌──┴───┐                       
                      │output│                       
                      └──────┘
```

## Requirements

- Roswell

- qlot

- protobuf

- I have confirmed cl-onnx works on macOS/Linux. I dunno about Windows😎.

## Installing (TODO)

- do not forget to update all submodules in advance.

```sh
$ make build
```

