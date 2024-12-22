;;;; cl-ollama.asd

(asdf:defsystem #:cl-ollama
  :description "An Ollama client for Common Lisp"
  :author "Vee Satayamas <vsatayamas@gmail.com>"
  :license  "APACHE-2.0"
  :version "0.0.2"
  :serial t
  :depends-on ("dexador" "com.inuoe.jzon" "utf8-input-stream")
  :components ((:file "package")
               (:file "cl-ollama")))
