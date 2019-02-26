;; NOTES:
;; Every client connecting to this server should have a unique id that cannot
;; be easily spoofed. It will use this id to confirm its identity.
;; Everything that the client does will be interpreted with this id in mind.
;; The client will be responsible for generating and providing that id.
;; It will be used as part of the context in the database.
;; Context will be: client_id, path (like server:#channel), caller (username)


(use-modules (ice-9 rdelim)
	     (ice-9 threads)
	     (ice-9 suspendable-ports)
	     (srfi srfi-1)
	     (rnrs io ports)
	     (ice-9 ports internal))

(install-suspendable-ports!)
(define (read-waiter port)
  (port-poll port "r" 500))
(define (write-waiter port)
  (port-poll port "w" 500))

;; TODO: make this atomic so we can clean it up asynchronously
(define connection-threads '())
(define (connection-threads-cleanup)
  (set! connection-threads
    (filter (lambda (thread)
	      (not (thread-exited? thread)))
	    connection-threads)))

(sigaction SIGINT
  (lambda (x)
    (display "Cancelling the connection threads...\n")
    (for-each (lambda (thread)
		; FIXME: this could throw and join-thread would hang
		;        indefinitely
		(cancel-thread thread))
	      connection-threads)
    (for-each (lambda (thread)
		(join-thread thread))
	      connection-threads)
    (display "All connections were closed.\n")
    (throw 'SIGINT x)))

(define (acquire-context-from-client client-port)
  (unless (port? client-port)
    (throw 'bad-client-port 'acquire-context-from-client
	   "client-port is not a port"))
  (catch 'return
    (lambda ()
      (parameterize ((current-read-waiter read-waiter)
		     (current-write-waiter write-waiter))
	;; ask the client for an id
	(display "SRV:ID" client-port)
	;; get the response
	(let ((response (get-line client-port)))
	  (format #t "Id response for ~a: \"~a\"\n" client-port response)
	  ;; if the incomming message is not an id, inform the client
	  ;; and return #f
	  (unless (string-prefix? "CLT:ID:" response)
	    (display "SRV:ID_BAD" client-port)
	    (throw 'return))
	  ;; else send ack message to a client and return the sent ID
	  (display "SRV:ID_ACK" client-port)
	  (substring/shared response 7))))
    (lambda (key . args) #f)))

(define (connection-handler client-connection)
  "CLIENT-CONNECTION should be a pair returned from the `accept' function."
  (unless (and (pair? client-connection)
	       (port? (car client-connection)))
    (throw 'bad-client-connection 'connection-handler
	   "client-connection is not a valid connection"))
  (let ((client (car client-connection))
	(client-id #f))
    (fcntl client F_SETFL (logior O_NONBLOCK
    				  (fcntl client F_GETFL)))
    (set-port-encoding! client "UTF-8")
    (dynamic-wind
      (lambda () #f)
      (lambda ()
	;; identify the client (acquire its context)
	;; TODO: Create a timeout for identification, after which the
	;;       connection is dropped
	(let ((client-context
	       (do ((context (acquire-context-from-client client)
			     (acquire-context-from-client client))
		    (num-tries 1 (1+ num-tries)))
		   ((or context (> num-tries 3)) context)
		 (sleep 1))))
	  (unless client-context
	    (throw 'client-wont-identify)) ; TODO: catch that exception
	  (parameterize ((current-read-waiter read-waiter))
	    ;; event loop to listen for requests from the client
	    (let ((exit? #f))
	      (while (not exit?)
		(let ((line (get-line client)))
		  (when (or (and (string? line)
	    			 (string=? (string-trim-right line) "exit"))
	    		    (eof-object? line))
	    	    (set! exit? #t))
		  (monitor
		   (format #t "~a: ~a\n" client-context line))))))))
      (lambda ()
	(monitor (format #t "Closing connection: ~a\n" client))
	(close-port client)))))

(catch #t
  (lambda ()
    (let* ((sockdir (string-append/shared
		     "/tmp/oxonbot"
		     (number->string (passwd:uid (getpw (getlogin))))))
	   (sockpath (string-append sockdir "/socket"))
	   (sock (socket PF_UNIX SOCK_STREAM 0)))
      (unless (file-exists? sockdir)
	(mkdir sockdir))
      (dynamic-wind
	(lambda () #f)			; dynamic wind in_guard
	(lambda ()			; dynamic wind thunk
	  (bind sock (make-socket-address AF_UNIX sockpath))
	  ;; set the listener socket to be non-blocking
	  (fcntl sock F_SETFL (logior O_NONBLOCK
  				      (fcntl sock F_GETFL)))
	  (listen sock 5)
	  (display "Listening for clients...\n")

	  (parameterize ((current-read-waiter read-waiter))
	    (let ((new-connection #f)
		  (exit? #f))
	      (while (not exit?)
		(set! new-connection (accept sock))
		(when new-connection
  		  (format #t "New connection: ~a\n" new-connection)
		  (set! connection-threads
		    (cons (begin-thread
			   (connection-handler new-connection))
			  connection-threads)))
		(connection-threads-cleanup)
		(format #t "Connections active: ~a\n"
			(length connection-threads))))))
	(lambda () 			; dynamic wind post_guard
	  (close sock)
	  (delete-file sockpath)))))
  ;; catch handler
  (lambda (key . args)
    (cond
      ((eq? key 'SIGINT) (display "Exit on keyboard interrupt.\n"))
      (else
       (throw key args)
       ;; (format #t "~a: ~a\n" key args)
       ))))
