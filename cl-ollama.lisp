;;;; cl-ollama.lisp

(in-package #:cl-ollama)

(defparameter *model-name* "llama3.1:8b")
(defparameter *protocol* "http")
(defparameter *host* "localhost")
(defparameter *port* 11434)
(defparameter *read-timeout* 300)

(setq utf8-input-stream::*line-buffer-size* 32)

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
	   (if (streamp ,raw-resp)
	       (let ((,s (utf8-input-stream:make-utf8-input-stream ,raw-resp)))
		 (loop for ,line = (read-line ,s nil nil)
		       while ,line
		       do
			  (let* ((,resp (com.inuoe.jzon:parse ,line)))
			    (progn
			      ,@body))))
	       (let* ((,resp (com.inuoe.jzon:parse ,raw-resp)))
		 (progn
		   ,@body))))))))

(defmacro generate ((resp prompt &key options keep-alive) &body body)
  (let ((key-values (gensym))
	(obj (gensym)))
    `(let ((,key-values '()))
       (push (cons "model" *model-name*) ,key-values)
       (push (cons "prompt" ,prompt) ,key-values)
       (when ,options
	 (push (cons "options" ,options) ,key-values))
       (when ,keep-alive
	 (push (cons "keep_alive" ,keep-alive) ,key-values))
       (do-ollama-request (,obj :post "generate" ,key-values)
			  (let ((,resp (gethash "response" ,obj)))
			    ,@body)))))

(defstruct message
  role
  content
  image)

(defun message-to-hash-tab (message)
  (let ((tab (make-hash-table :test #'equal)))
    (with-slots (role content image) message
      (setf (gethash "role" tab) role)
      (setf (gethash "content" tab) content)
      (when image
	(setf (gethash "image" tab) image)))
    tab))

(defun tab-to-plist-kw (tab)
  (loop with plist = '()
	for k being the hash-key of tab using (hash-value v)
	as k* = (intern (string-upcase k) :keyword)
	do
	   (push v plist)
	   (push k* plist)
	finally (return plist)))

(defmacro chat ((resp messages &key format options (stream t) keep-alive) &body body)
  (let ((key-values (gensym))
	(obj (gensym)))
    `(let ((,key-values '()))
       (push (cons "model" *model-name*) ,key-values)
       (push (cons "messages" (map 'list #'message-to-hash-tab ,messages))
	     ,key-values)
       (when ,format
	 (push (cons "format" ,format) ,key-values))
       (when ,options
	 (push (cons "options" ,options) ,key-values))
       (push (cons "stream" ,stream) ,key-values)
       (when ,keep-alive
	 (push (cons "keep_alive" ,keep-alive) ,key-values))
       (do-ollama-request (,obj :post "chat" ,key-values)
	 (let ((,resp (tab-to-plist-kw (gethash "message" ,obj))))
	   ,@body)))))
