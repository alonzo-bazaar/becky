(ql:quickload "usocket")
(load "request.lisp")
(load "render.lisp")
(load "parsereq.lisp")

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
  (multiple-value-bind (stream status req)
	  (parse-request (usocket:socket-stream client))
	  (declare (ignore stream))
  	(if (eq status 'ok)
		(print-request-back (usocket:socket-stream client) req)
  		(progn
  		  (princ "parsing request failed for the following reason:")
  		  (terpri)
  		  (princ status)
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

(server-loop :port 8082)

