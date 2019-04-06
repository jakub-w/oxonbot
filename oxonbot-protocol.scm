;;; Protocol specification:
;;;
;;; s       = ASCII SEPARATOR CHARACTER THAT IS NOT GROUP SEPARATOR
;;; content = unit *(s unit)
;;; subtype = DEPENDING ON type
;;; gs      = GROUP SEPARATOR ASCII CHARACTER
;;; us      = UNIT SEPARATOR ASCII CHARACTER
;;; etx     = END OF TEXT ASCII CHARACTER
;;; type    = "ID" / "Q" / "D" ; ID - id, Q - query, D - drop
;;; message = type gs subtype [gs content] etx
;;;
;;; subtypes for type (etx at the end is implied):
;;; ID:
;;;   REQ - request an ID
;;;   ACK - ID set succesfully
;;;   NAK - bad ID
;;;   RES - "ID" gs "RES" gs *charset - ID response with an id as a content
;;; Q:
;;;   ASK - "Q" gs "ASK" gs path us caller us *CHAR - ask the server for a
;;;                                                   response for the query
;;;   RES - "Q" gs "RES" gs path us caller us *CHAR - server response for the
;;;                                                   query
;;;   NAK - bad query
;;;   RDY - ready for accepting queries (Q:ASK)
;;; D:
;;;   ID - dropping the connection because ID cannot be obtained or recognized
;;;
;;; path can be any number of word and '/' characters
;;; caller can consist of only word characters

;; TODO: ob-msg could be a record with type, subtype, content fields

(add-to-load-path ".")
(define-module (oxonbot-protocol)
  #:use-module (ice-9 control)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 regex)
  #:use-module (oxonbot)
  #:use-module (srfi srfi-9)
  #:export (ob-msg?
	    get-message
	    ob-id-req
	    ob-id-ack
	    ob-id-nak
	    ob-id-extract
	    ob-request-id
	    ob-query-make-response
	    ob-query-make-ask
	    ob-query-nak
	    ob-query-ask-extract
	    ob-drop-id
	    ob-client
	    make-ob-client
	    ob-client?
	    ob-client-id
	    set-ob-client-id!
	    ob-client-port
	    set-ob-client-port!))
(use-modules (ice-9 textual-ports)
	     (ice-9 regex)
	     (oxonbot)
	     (srfi srfi-9))

(define-record-type ob-client
  (make-ob-client id port)
  ob-client?
  (id ob-client-id set-ob-client-id!)
  (port ob-client-port set-ob-client-port!))


(define (ob-client-valid? ob-client)
  (and (ob-client? ob-client)
       (string? (ob-client-id ob-client))
       (port? (ob-client-port ob-client))))

;; TODO: Maybe add an optional argument for a type of a message
(define (ob-msg? obj)
  (if (and (string? obj)
	   (string-match "^[A-Z]+\x1d[A-Z]+(\x1d.*)?\x03$" obj))
      #t
      #f))

(define (get-message port)
  (let* ((message '())
	 (str-msg
	  (do ((c (get-char port)
		  (get-char port)))
	      ((or (eof-object? c)
		   (begin
		     (set! message (cons c message))
		     (char=? c #\etx)))
	       (reverse-list->string message)))))
    (if (string-null? str-msg)
	#f
	str-msg)))

;; ID
(define ob-id-req "ID\x1dREQ\x03")
(define ob-id-ack "ID\x1dACK\x03")
(define ob-id-nak "ID\x1dNAK\x03")

(define (ob-id-extract message)
  (unless (string? message)
    (throw 'wrong-type-arg "ob-id-extract"
	   "Wrong type argument for MESSAGE: ~S" (list message)))
  (let ((message-match (string-match "^ID\x1dRES\x1d(\\w*)\x03$" message)))
    (if (eq? #f message-match)
	#f
	(match:substring message-match 1))))

(define (ob-request-id client-port)
  "Return the client's ID if identified correctly or #f."
  (unless (and (port? client-port) (not (port-closed? client-port)))
    (throw 'wrong-type-arg "ob-request-id"
	   "Wrong type argument for CLIENT-PORT: ~S" (list client-port)))
  ;; Send the ID request
  (display ob-id-req client-port)
  (let* ((response (get-message client-port))
	 (client-id (ob-id-extract response)))
    ;; (simple-format #t "Id response for ~A: ~S\n" client-port response)
    (if client-id
	(display ob-id-ack client-port)
	(display ob-id-nak client-port))
    client-id))
;; END OF ID

;; QUERY
(define (ob-query-make-response context response)
  "This is a server side function to create a response message to the
client's query.

CONTEXT must be a context record."
  (unless (ob-context? context)
    (throw 'wrong-type-arg "ob-query-make-response"
  	   "Wrong type argument for CONTEXT: ~S" (list context)))
  (simple-format #f "Q\x1dRES\x1d~A\x1f~A\x1f~A\x03"
		 (ob-context-path context)
		 (ob-context-caller context)
		 response))

(define (ob-query-make-ask path caller query)
  "This is a client side function to create a message to ask the server for
something."
  (simple-format #f "Q\x1dASK\x1d~A\x1f~A\x1f~A\x03" path caller query))

(define ob-query-nak "Q\x1dNAK\x03")
(define ob-query-rdy "Q\x1dRDY\x03") ;; ready for accepting queries

(define (ob-query-ask-extract ob-client message)
  "OB-CLIENT is an ob-client record. Its id has to be a string. Port must be
a port.
MESSAGE is an unprocessed ob-client message of type Q:ASK.

Return a pair in format (message-context . query) or #f if MESSAGE is
malformed or wrong type/subtype."
  (unless (ob-client-valid? ob-client)
    (throw 'wrong-type-arg "ob-query-ask-extract"
	   "OB-CLIENT is not valid (not a proper ob-client): ~S"
	   (list ob-client)))
  (call/ec
   (lambda (return)
     (when (< (string-length message) 1)
       (return #f))
     (unless (string= (string-take-right message 1) "\x03")
       (set! message (string-append/shared message "\x03" )))
     (unless (ob-msg? message)
       ;; (throw 'wrong-type-arg "ob-query-ask-extract"
       ;; 	   "Wrong type argument for MESSAGE: ~S" (list message))
       (return #f))
     ;; TODO/NOTE: this could be done by string-split and checking every value
     ;;            for correctness for better error-catching
     (let ((message-match
	    (string-match
	     "^Q\x1dASK\x1d([a-zA-Z0-9_\\/]+)\x1f(\\w+)\x1f(.*)\x03$"
	     message)))
       (if (eq? #f message-match)
	   (return #f)
	   (return
	    (cons
	     (make-ob-context
	      ;; client-id
	      (ob-client-id ob-client)
	      ;; path
	      (match:substring message-match 1)
	      ;; caller
	      (match:substring message-match 2))
	     ;; query
	     (match:substring message-match 3))))))))
;; END OF QUERY

;; DROP
(define ob-drop-id "D\x1dID\x03")
;; END OF DROP
