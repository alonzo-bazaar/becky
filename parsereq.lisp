(defun split-readline (stream)
  "reads line from stream then splits it on whitespace,
if line is empty (or all whitespace) it returns an empty list, ie nil
if eof was reached it returns the symbol 'eof "
  (let ((line (read-line stream nil 'eof)))
	(if (eq line 'eof) 'eof
		(remove-if (lambda (x) (= (length x) 0))
				   (uiop:split-string
					line
					:separator '(#\return #\linefeed #\space #\tab))))))

#|
the idea is to have all parser functions receive and return a tuple of the
following values
stream - status - partial-result
where status can be one of 'ok 'eof or some error code

this api can probably be refined, we may, for instance, want to return somewhat
richer error messages than just a symbol, because I am a fucking idiot and will
need all the error information I can get my hands on when developing this
goddamn thing

every function adds the shit it gotta add to the partial object returns it
plus a status
plus a stream from which the 

this is just an overengineered way to split a one pass parser into multiple
functions

I initially just tried passing the stream around but then it would
hang at if one function call tried to read form a stream that had given eof on
a previous function
because the second function call had no way to know that stream had already
given an eof
|#

(defun parse-request-first-line (stream status partial)
  "reads a line of <http method> <http request target> <http version>
from `stream' and sets the fields of the `partial' http request accordingly
if the read line is invalid the returned request will not be altered and an error status will be provided
returns (values `stream' `status-code' `updated-partial`), where:

`stream' is the stream it received as input, after consuming that first line

`status-code' is a symbol, either 'ok, 'eof if the streamed reach eof while reading the first line (which shouldn't really happen so...), or some error code

`updated-partial' is the object it received as input, but updated (this function is destructive, it won't create a new object, and update that, it will instead update the one it was passed, and return it for the next stage of parsing)"

  (declare (ignore status)) ;; dummy initial status passed for api consistency
  (declare (stream stream))
  (declare (http-request partial))

  (let ((line (split-readline stream)))
	(cond
	  ;; all the various way things can go wrong
	  ((eq line 'eof)
	   (values stream 'eof partial))
	  ((not (= (length line) 3))
	   (values stream 'malformed-first-line partial))
	  ((not (http-method-p (car line)))
	   (values stream 'invalid-http-method partial))
	  ((not (string= (caddr line) "HTTP/1.1"))
	   (values stream 'invalid-http-version partial))
	  ;; here all assertions pass
	  ;; three words long, <method> <url> HTTP/1.1
	  ;; TODO: check for url validity (?)
	  (t
	   (setf
		(http-request-method partial) (car line)
		(http-request-target partial) (cadr line)
		(http-version partial) (caddr line))
	   (values stream 'ok partial)))))

(defun parse-request-headers (stream status partial)
  "reads http header options from from `stream', if it reads valid message headers and correctly reaches an empty crlf line it inserts the various http message headers it read read into (http-request-headers partial)
if the read message headers are invalid the returned request will be not be altered and an error status will be provided

returns (values `stream' `status-code' `updated-partial`)

`stream' is the stream it received as input, after consuming all lines containing message headers, *and* the empty crlf line

`status-code' is a symbol, either 'ok, 'eof if the streamed reach eof while reading the first line (which shouldn't really happen so...), or some error code

`updated-partial' is the object it received as input, but updated (this function is destructive, it won't create a new object, and update that, it will instead update the one it was passed, and return it for the next stage of parsing)"
  (when (eq status 'eof)
	(return-from parse-request-headers (values stream 'premature-eof partial)))
  (unless (eq status 'ok)
	(return-from parse-request-headers (values stream status partial)))

  (labels ((valid-header-line-p (line)
			 (and (= (length line) 2)
				  (char= (char (car line) (1- (length (car line)))) #\:)))
		   (parse-header-line (line)
			 (cons (subseq (car line) 0 (1- (length (car line))))
				   (cadr line)))
		   (iter (acc)
			 (let ((l (split-readline stream)))
			   (cond
				 ;; empty crlf line, success
				 ((null l)
				  (setf (http-headers partial) (nreverse acc))
				  (values stream 'ok partial))
				 ;; eof before empty crlf line, no good
				 ((eq l 'eof)
				  (values stream 'premature-eof partial))
				 ((not (valid-header-line-p l))
				  (values stream 'invalid-header-line partial))
				 (t (iter (cons (parse-header-line l) acc)))))))
	(iter (list))))


#|
TODO:
I... might need to make this a generic, or something like that, a significant
amount of the point of this refactor in the request parsing was having
parse-request-body be able to handle shit a bit more generically
so streaming file uploads and shit

as of writing I think I'll still have parse-request-body still just
take everyting in a string
|#

(defun parse-request-body (stream status partial)
  (cond
	;; a previous eof, ie an empty body, is a valid input here
	((eq status 'eof)
	 (setf (http-body partial) (make-instance 'http-null-body))
	 (values stream 'eof partial))
	;; and propagate eventual errors as always
	((not (eq status 'ok))
	 (values stream status partial))
	;; if we're checking these it means the request has a body:
	;; only put, post, and patch requests can have a body, so we check

;	((not (member (http-request-method partial)
;				  '("PUT" "POST" "PATCH")
;				  :test #'string=))
;	 (values stream 'method-cannot-have-body partial))

	;; request with body that can have a body
	;; (more cases to be added once I add support for file upload and shit)
	(t
	 (let* ((lines
			  (do ((l (read-line stream nil 'eof)
					  (read-line stream nil 'eof))
				   (acc nil (cons l acc)))
				  ((or (eq l 'eof) (char= (char l 0) #\return))
				   (nreverse acc))))
			(body (if lines
					  (make-instance 'http-plaintext-body :plaintext lines)
					  (make-instance 'http-null-body))))
	   (setf (http-body partial) body)
	   (values stream 'ok partial)))))

(defun parse-request (stream)
  "create http request object by reading input stream, returns
(values stream 'ok <the request>) if parsing was successful,
(values stream <error message> <partially parsed request>) otherwise"
  (multiple-value-call #'parse-request-body
	  (multiple-value-call #'parse-request-headers
		  (parse-request-first-line stream nil (make-instance 'http-request)))))

;;; unit tests (to be later made into a fiveam thing)
;; parse-request-first-line
(multiple-value-bind (str status r)
	(with-input-from-string (s "POST / HTTP/1.1")
	  (parse-request-first-line s nil (make-instance 'http-request)))
  (declare (ignore str))
  (assert (string= (http-request-method r) "POST"))
  (assert (string= (http-request-target r) "/"))
  (assert (string= (http-version r) "HTTP/1.1"))
  (assert (eq status 'ok)))

(multiple-value-bind (str status r)
	(with-input-from-string (s "/ HTTP/1.1 POST")
	  (parse-request-first-line s nil (make-instance 'http-request)))
  (declare (ignore str r))
  (assert (eq status 'invalid-http-method)))

(multiple-value-bind (str status r)
	(with-input-from-string (s "POST / HTTP/1.2")
	  (parse-request-first-line s nil (make-instance 'http-request)))
  (declare (ignore str r))
  (assert (eq status 'invalid-http-version)))

(multiple-value-bind (str status r)
	(with-input-from-string (s "POST / / HTTP/1.1")
	  (parse-request-first-line s nil (make-instance 'http-request)))
  (declare (ignore str r))
  (assert (eq status 'malformed-first-line)))

(multiple-value-bind (str status r)
	(with-input-from-string (s "POST  HTTP/1.1")
	  (parse-request-first-line s nil (make-instance 'http-request)))
  (declare (ignore str r))
  (assert (eq status 'malformed-first-line)))

;; https://stackoverflow.com/questions/2619172/common-lisps-equivalent-of-r-inside-the-format-function
;; not the most efficient implementation out there :|
(defun crlf-separate (&rest strings)
  (apply #'concatenate 'string
		 (reduce #'append
				 (mapcar (lambda (x) (list x #(#\return #\linefeed)))
						 strings))))

;; parse-request-headers
(multiple-value-bind (str status r)
	(with-input-from-string (s (crlf-separate "Content-Type: go/fuck/yourself"
											  "Fuck: you_bitch"
											  "Nigerundayo: smokey"
											  ""))
	  (parse-request-headers s 'ok (make-instance 'http-request)))
  (declare (ignore str))
  (let ((head (http-headers r)))
	(assert (string= (cdr (assoc "Content-Type" head :test #'string=))
					 "go/fuck/yourself"))
	(assert (string= (cdr (assoc "Fuck" head :test #'string=))
					 "you_bitch"))
	(assert (string= (cdr (assoc "Nigerundayo" head :test #'string=))
					 "smokey")))
  (assert (eq status 'ok)))

;; notice the lack of final empty string
(multiple-value-bind (str status r)
	(with-input-from-string (s (crlf-separate "Content-Type: go/fuck/yourself"
											  "Fuck: you_bitch"
											  "Nigerundayo: smokey"))
	  (parse-request-headers s 'ok (make-instance 'http-request)))
  (declare (ignore r str))
  (assert (eq status 'premature-eof)))

;; TODO
;; parse-body
