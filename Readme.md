# cl-onnx

ONNX Parser dedicated to [Caten](https://github.com/hikettei/Caten). (If you are looking for ONNX Runtime running with Common Lisp, visit Caten.)

## Systems

``` sh
(ql:quickload :cl-onnx) ;; running with minimal dependencies
(ql:quickload :cl-onnx/viz) ;; comes with extra dependencies but objects are displayed in more readable format.
```

## Feature: CLI Netron

I am too lazy to open netron even for the small graphs :(. So, just for my fun, I tried creating a CLI tool for graph visualization similar to Netron. However, it requires additional dependencies, so it won't work unless you do (ql:quickload :cl-onnx/viz).

```
;; (ql:quickload :cl-onnx/viz) to use pprint graph.
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

## Installing (TODO)

- do not forget to update all submodules in advance.

```sh
$ make build
```

