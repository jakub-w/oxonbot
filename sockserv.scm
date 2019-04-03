;; NOTES:
;; Every client connecting to this server should have a unique id that cannot
;; be easily spoofed. It will use this id to confirm its identity.
;; Everything that the client does will be interpreted with this id in mind.
;; The client will be responsible for generating and providing that id.
;; It will be used as part of the context in the database.
;; Context will be: client_id, path (like server:#channel), caller (username)

(add-to-load-path ".")
(use-modules (ice-9 rdelim)
	     (ice-9 threads)
	     (ice-9 suspendable-ports)
	     (srfi srfi-1)
	     (srfi srfi-9)
	     (rnrs io ports)
	     (ice-9 ports internal)
	     (oxonbot)
	     (oxonbot-protocol)
	     (system repl error-handling))

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

(sigaction SIGPIPE
  ;; (lambda (x)
  ;;   (display "Pipe was broken. Client dropped the connection?\n"
  ;; 	     (current-error-port)))
  SIG_IGN)

(define (connection-handler client-connection)
  "This should be used in a separate thread.

CLIENT-CONNECTION should be a pair returned from the `accept' function."
  (unless (and (pair? client-connection)
	       (port? (car client-connection)))
    ;; FIXME: change to wrong-type-arg
    (throw 'bad-client-connection 'connection-handler
	   "client-connection is not a valid connection"))
  (with-throw-handler #t
    (lambda ()
      (let ((client-port (car client-connection)))
       (fcntl client-port F_SETFL (logior O_NONBLOCK
    					  (fcntl client-port F_GETFL)))
       (set-port-encoding! client-port "UTF-8")
       (dynamic-wind
	 (lambda () #f)
	 (lambda ()
	   ;; TODO: Create a timeout for identification, after which the
	   ;;       connection is dropped
	   ;; Identify the client (acquire its context).
	   ;; Try 3 times before dropping the connection.
	   (catch #t
	     (lambda ()
	       (let ((ob-client
		      (do ((id (ob-request-id client-port)
			       (ob-request-id client-port))
			   (num-tries 1 (1+ num-tries)))
			  ((or id (> num-tries 3)) (cons id client-port))
			(sleep 1))))
		 (unless (car ob-client)
		   (throw 'client-wont-identify))
		 (format #t "Client identified as: ~S\n"
			 (car ob-client))
		 (parameterize ((current-read-waiter read-waiter))
		   ;; event loop to listen for requests from the client
		   (while #t
		     (let ((line (get-line client-port)))
		       (when (or
			      (and (string? line)
	    			   (string=? (string-trim-right line) "exit"))
	    		      (eof-object? line))
			 (break))
		       (monitor (format #t "~a: ~s\n" (car ob-client) line))
		       (let ((query (ob-query-ask-extract ob-client line)))
			 (if query
			     ;; let oxonbot write to string, then send the
			     ;; response to the client
			     (display (with-output-to-string
					(lambda ()
					  (oxonbot-command (car query)
							   (cdr query))))
				      client-port)
			     (begin
			       (display ob-query-nak client-port)
			       (format (current-error-port)
				       "Bad query from client ~S: ~S\n"
				       (car ob-client) line)))))))))
	     (lambda (key . args) ;; catching only 'client-wont-identify
	       (cond
		((eq? key 'client-wont-identify)
		 (monitor (format (current-error-port)
				  "Client on port ~a won't identify.\n"
				  client-port))
		 (display ob-drop-id client-port))
		((and (eq? key 'system-error)
		      (let ((errno (system-error-errno (cons key args))))
			(or (= errno 104)
			    (= errno 32))))
		 (monitor (display "Client disconnected.\n")))

		(else #f
		      (monitor (format (current-error-port)
	      			       "Error: ~a: ~a\n"
	      			       key args)))))))
	 (lambda ()
	   (monitor (format #t "Closing connection: ~a\n" client-port))
	   (close-port client-port)))))
    (lambda args
      (display (backtrace))
      (newline))))

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
       (format #t "~a: ~a\n" key args)
       ;; (throw key args)
       ))))
