;;;; package.lisp

(defpackage #:cl-ollama
  (:use #:cl)
  (:export *model-name* *protocol* *host* *port* *read-timeout* unable-to-fetch-data
	   #:chat
	   #:do-chat
	   #:generate
	   #:do-generate
	   #:create-model
	   #:do-create-model
	   #:create-blob
	   #:list-local-models
	   #:show-model-information
	   #:copy-model
	   #:delete-model
	   #:pull-model
	   #:do-pull-model
	   #:push-model
	   #:do-push-model
	   #:generate-embeddings))
