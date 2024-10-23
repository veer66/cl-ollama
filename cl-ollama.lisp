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

(defun build-params-hash-tab (params)
  (let ((param-hash-tab (make-hash-table :test #'equal)))
    (loop for (k . v) in params
	  do
	     (setf (gethash k param-hash-tab) v)
	  finally
	     (return param-hash-tab))))

(defun build-params-text (params)
  (com.inuoe.jzon:stringify (build-params-hash-tab params)))

(defun tab-to-plist-kw (tab)
  (loop with plist = '()
	for k being the hash-key of tab using (hash-value v)
	as k* = (intern (string-upcase k) :keyword)
	do
	   (push v plist)
	   (push k* plist)
	finally (return plist)))

(defun kw-plist-to-tab (kw-plist)
  (loop with tab = (make-hash-table :test #'equal)
	for (k v) on kw-plist
	  by #'cddr
	do
	   (setf (gethash (string-downcase (symbol-name k)) tab)
		 v)
	finally
	   (return tab)))

(defun request (method verb params process-response)
  (let ((params-text (build-params-text params))
	(url (gen-url verb)))
    (multiple-value-bind (raw-resp status)
	(dex:request url
		     :method method
		     :content params-text
		     :read-timeout *read-timeout*
		     :want-stream t)
      (unless (= status 200)
	(error 'unable-to-fetch-data
	       :want-stream t
	       :http-method method
	       :url url
	       :params params
	       :status-code status))
      (if (streamp raw-resp)
	  (let ((s (utf8-input-stream:make-utf8-input-stream raw-resp)))
	    (loop for line = (read-line s nil nil)
		  while line
		  do
		     (let* ((resp (com.inuoe.jzon:parse line)))
		       (funcall process-response (tab-to-plist-kw resp)))))
	  (let ((resp (com.inuoe.jzon:parse raw-resp)))
	    (funcall process-response (tab-to-plist-kw resp)))))))

(defun build-params-for-generation (prompt &key options keep-alive)
  (let ((params '()))
    (push (cons "model" *model-name*) params)
    (push (cons "prompt" prompt) params)
    (when options
      (push (cons "options" options) params))
    (when keep-alive
      (push (cons "keep_alive" keep-alive) params))
    params))

(defun generate (prompt process-response &key options keep-alive)
  (request :post
	   "generate"
	   (build-params-for-generation prompt
					:options options
					:keep-alive keep-alive)
	   process-response))

(defmacro do-generate ((resp prompt &key options keep-alive) &body body)
  `(generate ,prompt
     (lambda (,resp)
       ,@body)
     :options ,options
     :keep-alive ,keep-alive))


(defun build-params-for-chat (messages &key format options (stream t) keep-alive)
  (let ((params '()))
    (push (cons "model" *model-name*) params)
    (push (cons "messages" (map 'list #'kw-plist-to-tab messages))
	  params)
    (when format
      (push (cons "format" format) params))
    (when options
      (push (cons "options" options) params))
    (push (cons "stream" stream) params)
    (when keep-alive
      (push (cons "keep_alive" keep-alive) params))
    params))

(defun chat (messages process-response &key format options (stream t) keep-alive)
  (request :post
	   "chat"
	   (build-params-for-chat messages
				  :format format
				  :options options
				  :stream stream
				  :keep-alive keep-alive)
	   (lambda (resp)
	     (let ((resp* (copy-list resp)))
	       (setf (getf resp* :MESSAGE)
		     (tab-to-plist-kw (getf resp* :MESSAGE)))
	       (funcall process-response resp*)))))

(defmacro do-chat ((resp messages &key format options (stream t) keep-alive) &body body)
  `(chat ,messages
     (lambda (,resp)
       ,@body)
     :format ,format
     :options ,options
     :stream ,stream
     :keep-alive ,keep-alive))

(defun build-params-for-creating-model (name modelfile)
  (let ((params '()))
    (push (cons "name" name) params)
    (push (cons "modelfile" modelfile) params)
    params))

(defun create (name modelfile process-response)
  (request :post
	   "create"
	   (build-params-for-creating-model name modelfile)
	   process-response))

(defmacro do-create ((resp name modelfile) &body body)
  `(create ,name
	   ,modelfile
	   (lambda (,resp)
	     ,@body)))
