(ql:quickload "usocket")

;; https://stackoverflow.com/questions/2619172/common-lisps-equivalent-of-r-inside-the-format-function
;; not the most efficient implementation out there :|
(defun crlf-separate (&rest strings)
  (apply #'concatenate 'string
		 (reduce #'append
				 (mapcar (lambda (x) (list x #(#\return #\linefeed)))
						 strings))))

;;; preamble of shit needed define http requests and responses
;; method
(defparameter http-methods-list
  '("CONNECT" "DELETE" "GET" "HEAD" "OPTIONS" "PATCH" "POST" "PUT" "TRACE"))
(defun http-method-p (method)
  (member method http-methods-list :test #'string=))

(defclass http-body ()
  ((mime-type
	:type string
	:accessor http-body-mime-type)))
(defclass http-null-body (http-body)
  ())
(defclass http-json-body (http-body)
  ((mime-type
	:initform "application/json")))
(defclass http-xml-body (http-body)
  ((mime-type
	:initform "application/xml")))
(defclass http-plaintext-body (http-body)
  ((mime-type
	:initform "text/plain")
   (plaintext
	:type string
	:initarg :plaintext
	:accessor http-plaintext-body-plaintext)))

(defgeneric render (body))
(defmethod render ((body http-plaintext-body))
  (apply #'crlf-separate (http-plaintext-body-plaintext body)))
(defmethod render ((body http-null-body))
  (declare (ignore body))
  "")

;;; requests and responses
;; formats as specified in
;; https://developer.mozilla.org/en-US/docs/Web/HTTP/Guides/Messages
(defclass http-message ()
  (;; only HTTP/1.1 but hey, we're trying, stored as "1.1"
   (version
	:type string
	:initarg :version
	:accessor http-version)
   ;; headers stored as an alist, for example '(("Content-Type" . "text/html"))
   (headers
	:type list
	:initarg :headers
	:accessor http-headers)
   (body
	:type http-body
	:initarg :body
	:accessor http-body)))

(defclass http-request (http-message)
  (;; 'get 'post 'whatever
   (target
	:type string
	:initarg :target
	:accessor http-request-target)
   ;; shit like /, /users, /app/login, /info/logan/dicksize, and the like
   (method
	:type string
	:initarg :method
	:accessor http-request-method)))

(defclass http-response (http-message)
  (;; 200, 404, those ones
   (code
	:type fixnum
	:initarg :code
	:accessor http-response-code)))

(defun split-readline (stream)
  "reads line from stream then splits it on whitespace,
if line is empty or stream has reached eof it returns a big phat nil"
  (let ((line (read-line stream nil 'eof)))
	(and (not (eq line 'eof))
		 (remove-if (lambda (x) (= (length x) 0))
					(uiop:split-string
					 line
					 :separator '(#\return #\linefeed #\space #\tab))))))

(defun parse-request-first-line (stream)
  "returns a list of <http method> <http request target> <http version>
if that's how the first line in `stream' is actually formatted,
if that's not how the first line is formatted, or if anything fails in the read, it returns `nil'"
  (let ((line (split-readline stream)))
	;; line should be
	;; <method> <target> HTTP/1.1
	(and (= (length line) 3)
		 (http-method-p (car line))
		 (string= (caddr line) "HTTP/1.1")
		 line)))

(defun parse-request-headers (stream)
  "reads http header options from from `stream' and returns an alist containing
the various http message header options it read
if an error happens during the read, or the function receives input that doesn't look like an http header option, it returns `nil'
to be called once the stream has avanced to just before the header of an http message, and will consume stream until an empty line is reached"
  (labels ((valid-line-p (line)
			 (and (= (length line) 2)
				  (char= (char (car line) (1- (length (car line)))) #\:)))

		   (parse-line (line)
			 (if (valid-line-p line)
				 (list (subseq (car line) 0 (1- (length (car line))))
					   (cadr line))
				 (return-from parse-request-headers nil))))

	(do ((line (split-readline stream) (split-readline stream))
		 (acc nil (cons (parse-line line) acc)))
		((null line) (nreverse acc)))))

(defun parse-request-body (stream first-line headers)
  (declare (ignore headers)) 
  (let ((method (car first-line)))
	;; only these three methods admit a request body
	(if (or (string= method "PATCH")
			(string= method "POST")
			(string= method "PUT"))
		(let ((lines
				(do ((l (read-line stream nil 'eof)
						(read-line stream nil 'eof))
					 (acc nil (cons l acc)))
					((or (eq l 'eof) (char= (char l 0) #\return)) (nreverse acc)))))
		  (princ "done") (terpri)
		  (if lines
			  (make-instance 'http-plaintext-body :plaintext lines)
			  (make-instance 'http-null-body))))
	(make-instance 'http-null-body)))


(defun parse-request (stream)
  "create http request object by reading input stream, returns
(values t <parsed request>) if parsing was successful,
or (values nil <error message>) if parsing was not succesful"
  (let* ((first-line (or (parse-request-first-line stream)
						 (return-from parse-request
						   (values nil "failed to parse first line of request"))))
		 (headers (or (parse-request-headers stream)
					  (return-from parse-request
						(values nil "failed to parse request headers"))))
		 (body (or (parse-request-body stream first-line headers)
				   (return-from parse-request
					 (values nil "failed to parse request body")))))

	(values t
			(make-instance 'http-request :method (car first-line)
										 :target (cadr first-line)
										 :version (caddr first-line)
										 :headers headers
										 :body body))))

(defun print-request-back (stream request)
  (let* ((content
		   (crlf-separate
			"http request with protocol version: " (http-version request) ""
			"for resource: " (http-request-target request) ""
			"with method: " (http-request-method request) ""
			"header options are"
			(apply #'crlf-separate
				   (mapcar (lambda (x) (format nil "<~A> : <~A>" (car x) (cadr x)))
						   (http-headers request)))
			"body is"
			(render (http-body request))))
		 (content-length (length content)))

	(write-string
	 (crlf-separate
	  "HTTP/1.1 200 OK"
	  "Content-Type: text/plain"
	  (format nil "Content-Length: ~A" content-length)
	  "")
	 stream)
	(write-string content stream)))

(defun response-to (client)
  (multiple-value-bind (ok data) (parse-request (usocket:socket-stream client))
  	(if ok
		(print-request-back (usocket:socket-stream client) data)
  		(progn
  		  (princ "parsing request failed for the following reason:")
  		  (terpri)
  		  (princ data)
  		  (terpri)))))

(defun server-loop (&key (host #(127 0 0 1)) (port 8080) (ttl 4))
  (usocket:with-socket-listener (sock host port)
	(dotimes (i ttl)
	  (format t "and a ~A~%" i) 
	  (usocket:with-connected-socket
		  (client (usocket:socket-accept sock
										  :element-type 'character))
		(write-string (response-to client) (usocket:socket-stream client))
		(force-output (usocket:socket-stream client))))))

; (server-loop)

