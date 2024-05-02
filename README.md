# cl-ollama
### _Vee Satayamas <vsatayamas@gmail.com>_

_cl-ollama_ is an Ollama client for Common Lisp.

## Prerequisites

* Ollama
* SBCL
* Quicklisp
* Ultralisp

## Example

### Basic usage

```Lisp
CL-USER> (ql:quickload "cl-ollama")
To load "cl-ollama":
  Load 1 ASDF system:
    cl-ollama
; Loading "cl-ollama"
...................
("cl-ollama")
CL-USER> (cl-ollama:generate (r "What is an interpreter in computer science?") (princ r))
In computer science, an interpreter is a software component that directly ...
...
...
CL-USER>
```

### How to switch to another model

```Lisp
CL-USER> (ql:quickload "cl-ollama" :silent t)
("cl-ollama")
CL-USER> (setq cl-ollama:*model-name* "phi3")
"phi3"
CL-USER> (cl-ollama:generate (r "What is an interpreter in computer science?") 
	   (princ r))
```

## License

APACHE-2.0

