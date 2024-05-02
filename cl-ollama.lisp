;;;; cl-ollama.lisp

(in-package #:cl-ollama)

(defparameter *model-name* "llama3")
(defparameter *protocol* "http")
(defparameter *host* "localhost")
(defparameter *port* 11434)
(defparameter *read-timeout* 300)

(defun gen-url (verb)
  (format nil "~a://~a:~a/api/~a" *protocol* *host* *port* verb))

(define-condition unable-to-fetch-data (error)
  ((status-code :initarg :status-code
		:reader status-code)
   (params :initarg :params
	   :reader params)
   (url :initarg :url
	:reader url)
   (http-method :initarg :http-method
		:reader http-method)
   (want-stream :initarg :want-stream
		:reader want-stream)))

(defmacro do-ollama-request ((resp method verb key-values) &body body)
  (let ((params (gensym))
	(params-text (gensym))
	(raw-resp (gensym))
	(status (gensym))
	(line (gensym))
	(s (gensym))
	(k (gensym))
	(v (gensym))
	(obj (gensym))
	(url (gensym)))
    `(let ((,params (make-hash-table :test #'equal)))
       (loop for (,k . ,v) in ,key-values
	     do
		(setf (gethash ,k ,params) ,v))
       (let ((,params-text (com.inuoe.jzon:stringify ,params))
	     (,url (gen-url ,verb)))
	 (multiple-value-bind (,raw-resp ,status)
	     (dex:request ,url
			  :method ,method
			  :content ,params-text
			  :read-timeout *read-timeout*
			  :want-stream t)
	   (unless (= ,status 200)
	     (error 'unable-to-fetch-data
		    :want-stream t
		    :http-method ,method
		    :url ,url
		    :params ,params
		    :status-code ,status))
	   (let ((,s (utf8-input-stream:make-utf8-input-stream ,raw-resp)))
	     (loop for ,line = (read-line ,s nil nil)
		   while ,line
		   do
		      (let* ((,obj (com.inuoe.jzon:parse ,line))
			     (,resp (gethash "response" ,obj)))
			(progn
			  ,@body)))))))))

(defmacro generate ((resp prompt &key options keep-alive) &body body)
  (let ((key-values (gensym)))
    `(let ((,key-values '()))
       (push (cons "model" *model-name*) ,key-values)
       (push (cons "prompt" ,prompt) ,key-values)
       (when ,options
	 (push (cons "options" ,options) ,key-values))
       (when ,keep-alive
	 (push (cons "keep_alive" ,keep-alive) ,key-values))
       (do-ollama-request (,resp :post "generate" ,key-values)
	 (progn ,@body)))))
