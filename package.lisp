;;;; package.lisp

(defpackage #:cl-ollama
  (:use #:cl)
  (:export *model-name* *protocol* *host* *port* *read-timeout* unable-to-fetch-data
	   #:generate #:chat #:message #:make-message))
