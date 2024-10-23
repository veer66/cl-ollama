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
CL-USER> (cl-ollama:do-create-model (r "my-model-2" "FROM llama3.2")
       (print r))

(:STATUS
 "using existing layer sha256:dde5aa3fc5ffc17176b5e8bdc82f587b24b2678c6c66101bf7da77af9f7ccdff")
...
```

### Create a Blob

```Lisp
CL-USER> (cl-ollama:create-blob #P"/a/path/to/llm/llama-2-7b.Q4_K_M.gguf" "sha256:4567208c2221da5a9f2ded6cc26ce58dd47d0410902c3f57a4a3ed104ce51b0b")
#()
201 (8 bits, #xC9, #o311, #b11001001)
#<HASH-TABLE :TEST EQUAL :COUNT 2 {1024CC3CF3}>
#<QURI.URI.HTTP:URI-HTTP http://localhost:11434/api/blobs/sha256:4567208c2221da5a9f2ded6cc26ce58dd47d0410902c3f57a4a3ed104ce51b0b>
NIL
```

### List local models

```Lisp
CL-USER> (cl-ollama:list-local-models)
(:MODELS
 (:DETAILS
  (:QUANTIZATION_LEVEL "Q4_K_M" :PARAMETER_SIZE "3.2B" :FAMILIES #("llama")
   :FAMILY "llama" :FORMAT "gguf" :PARENT_MODEL "")
  :DIGEST "c8cb353161296fb7fd1a0507b8693bd0ab9f9dc066b3b55057032e5acbfa0911"
  :SIZE 2019393189 :MODIFIED_AT "2024-10-23T11:38:33.552233884+07:00" :MODEL
  "my-model-3:latest" :NAME "my-model-3:latest")
...
```

### Show model information

```Lisp
CL-USER> (cl-ollama::show-model-information "llama3.2")
(:MODIFIED_AT "2024-10-23T10:54:07.678378774+07:00" :MODEL_INFO
 (:TOKENIZER.GGML.TOKENS NULL :TOKENIZER.GGML.TOKEN_TYPE NULL
  :TOKENIZER.GGML.PRE "llama-bpe" :TOKENIZER.GGML.MODEL "gpt2"
...
```

### Copy a model

```Lisp
CL-USER> (cl-ollama:copy-a-model "llama3.2" "m2")
#()
200 (8 bits, #xC8, #o310, #b11001000)
#<HASH-TABLE :TEST EQUAL :COUNT 2 {1036E91C93}>
#<QURI.URI.HTTP:URI-HTTP http://localhost:11434/api/copy>
NIL
```

### Delete a model

```Lisp
CL-USER> (cl-ollama:delete-a-model "m2")
#()
200 (8 bits, #xC8, #o310, #b11001000)
#<HASH-TABLE :TEST EQUAL :COUNT 2 {1028A53BF3}>
#<QURI.URI.HTTP:URI-HTTP http://localhost:11434/api/delete>
NIL
```

## License

APACHE-2.0
