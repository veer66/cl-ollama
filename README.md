# cl-ollama
### _Vee Satayamas <vsatayamas@gmail.com>_

_cl-ollama_ is an Ollama client for Common Lisp.

## Status

WIP

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
CL-USER> (cl-ollama:do-generate (r "What is an interpreter in computer science?") (print r))

(:DONE NIL :RESPONSE "In" :CREATED_AT "2024-10-20T14:31:18.642302463Z" :MODEL
 "llama3.1:8b")
...
```

### How to switch to another model

```Lisp
CL-USER> (ql:quickload "cl-ollama" :silent t)
("cl-ollama")
CL-USER> (setq cl-ollama:*model-name* "phi3")
"phi3"
CL-USER> (cl-ollama:do-generate (r "What is an interpreter in computer science?") (print r))

(:DONE NIL :RESPONSE "An" :CREATED_AT "2024-10-20T14:35:46.316081045Z" :MODEL
 "phi3")
...
```

### Chat

```Lisp
CL-USER> (ql:quickload "cl-ollama" :silent t)
("cl-ollama")
CL-USER> (cl-ollama::do-chat (r (list '(:role "user"
				                        :content "Why does the sky is yellow?")))
	        (print r))

(:DONE NIL :MESSAGE (:CONTENT "The" :ROLE "assistant") :CREATED_AT
 "2024-10-20T14:37:17.289970817Z" :MODEL "llama3.1:8b")

...
```

### Create a model

```Lisp
CL-USER> (cl-ollama:do-create (r "my-model-2" "FROM llama3.2")
	   (print r))

(:STATUS
 "using existing layer sha256:dde5aa3fc5ffc17176b5e8bdc82f587b24b2678c6c66101bf7da77af9f7ccdff")
...
```

## License

APACHE-2.0

