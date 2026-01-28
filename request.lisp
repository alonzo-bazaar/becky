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

;;; requests and responses
;; formats as specified in
;; https://developer.mozilla.org/en-US/docs/Web/HTTP/Guides/Messages
(defclass http-message ()
  (;; only HTTP/1.1 but hey, we're trying, stored as "HTTP/1.1"
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
