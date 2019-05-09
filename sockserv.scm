;; NOTES:
;; Every client connecting to this server should have a unique id that cannot
;; be easily spoofed. It will use this id to confirm its identity.
;; Everything that the client does will be interpreted with this id in mind.
;; The client will be responsible for generating and providing that id.
;; It will be used as part of the context in the database.
;; Context will be: client_id, path (like server:#channel), caller (username)

;; COMMAND LINE ARGUMENTS:
;; none - listen on both local and tcp sockets
;; "tcp" - listen only on tcp socket
;; "local" - listen only on local socket

;; TODO: add support for TCP sockets (for docker)

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
    (throw 'wrong-type-arg "connection-handler"
	   "Wrong type argument for CLIENT-CONNECTION: ~S"
	   (list client-connection)))
  (with-throw-handler #t
    (lambda ()
      (let ((client (make-ob-client #f (car client-connection))))
	(fcntl (ob-client-port client)
	       F_SETFL
	       (logior O_NONBLOCK (fcntl (ob-client-port client) F_GETFL)))
       (set-port-encoding! (ob-client-port client) "UTF-8")
       (dynamic-wind
	 (lambda () #f)
	 (lambda ()
	   (catch #t
	     (lambda ()
	       ;; TODO: Create a timeout for identification, after which the
	       ;;       connection is dropped
	       ;; TODO: This could be moved to a separate function
	       ;; Identify the client (acquire its context).
	       ;; Try 3 times before dropping the connection.
	       (do ((id (ob-request-id (ob-client-port client))
			(ob-request-id (ob-client-port client)))
		    (num-tries 1 (1+ num-tries)))
		   ((or id (> num-tries 3)) (set-ob-client-id! client id))
		 (sleep 1))
	       (unless (ob-client-port client)
		 (throw 'client-wont-identify))
	       ;; end of client id
	       (format #t "Client identified as: ~a\n"
		       (ob-client-id client))
	       (parameterize ((current-read-waiter read-waiter))
		 ;; event loop to listen for requests from the client
		 (while #t
		   (let ((line (get-message (ob-client-port client))))
		     (when (or
			    (and (string? line)
	    			 (string=? (string-trim-right line) "exit"))
	    		    (eof-object? line)
			    (not line))
		       (break))
		     (monitor (format #t "~a: ~s\n"
				      (ob-client-id client)
				      line))
		     ;; query is a pair (context . command)
		     (let ((query (ob-query-ask-extract client line)))
		       (if query
			   (display (ob-query-make-response
				     (car query)
				     (oxonbot-command (car query)
			     			      (cdr query)))
				    (ob-client-port client))
			   (begin
			     (display ob-query-nak (ob-client-port client))
			     (format (current-error-port)
				     "Bad query from client ~S: ~S\n"
				     (ob-client-id client) line))))))))
	     (lambda (key . args) ;; catching only 'client-wont-identify
	       (cond
		((eq? key 'client-wont-identify)
		 (monitor (format (current-error-port)
				  "Client on port ~a won't identify.\n"
				  (ob-client-port client)))
		 (display ob-drop-id (ob-client-port client)))
		((and (eq? key 'system-error)
		      (let ((errno (system-error-errno (cons key args))))
			(or (= errno ECONNRESET)
			    (= errno EPIPE))))
		 (monitor (format #t "Client ~s disconnected.\n"
				  (or (ob-client-id client)
				      (ob-client-port client)))))

		(else #f
		      (monitor (format (current-error-port)
	      			       "Error: ~a: ~a\n"
	      			       key args)))))))
	 (lambda ()
	   (monitor (format #t "Closing connection: ~s\n"
			    (or (ob-client-id client)
				(ob-client-port client))))
	   (close-port (ob-client-port client))))))
    (lambda args
      (display (backtrace))
      (newline))))

(catch #t
  (lambda ()
    (let* ((sockdir (string-append/shared
		     "/tmp/oxonbot"
		     (number->string (passwd:uid (getpw (getlogin))))))
	   (sockpath (string-append sockdir "/socket"))
	   ;; in the child the INET socket will be open instead of UNIX
	   (child-pid (or
		       ;; if a command-line argument states the type of a
		       ;; connection to be opened - don't fork
		       ;; instead assign zero or non-zero to child-pid
		       (and (> (length (program-arguments)) 1)
			    (let ((arg (cadr (program-arguments))))
			      (cond ((string= arg "tcp") 0)
				    ((string= arg "local") 1)
				    (else #f))))
		       (primitive-fork)))
	   (sock (if (zero? child-pid)
		     (socket PF_INET SOCK_STREAM 0)
		     (socket PF_UNIX SOCK_STREAM 0))))
      ;; INET socket doesn't need sockdir
      (unless (or (file-exists? sockdir) (zero? child-pid))
	(mkdir sockdir))
      (dynamic-wind
	(lambda () #f)			; dynamic wind in_guard
	(lambda ()			; dynamic wind thunk
	  (if (zero? child-pid)
	      (bind sock AF_INET INADDR_ANY 4563)
	      (bind sock AF_UNIX sockpath))
	  ;; set the listener socket to be non-blocking
	  (fcntl sock F_SETFL (logior O_NONBLOCK
  				      (fcntl sock F_GETFL)))
	  (listen sock 5)
	  (monitor (format #t "~A: Listening for clients...\n"
			   (if (zero? child-pid)
			       "TCP/IP"
			       "LOCAL")))

	  (parameterize ((current-read-waiter read-waiter))
	    (let ((new-connection #f)
		  (exit? #f))
	      (while (not exit?)
		(set! new-connection (accept sock))
		(when new-connection
  		  (monitor (format #t "New connection: ~a\n" new-connection))
		  (set! connection-threads
		    (cons (begin-thread
			   (connection-handler new-connection))
			  connection-threads)))
		(connection-threads-cleanup)
		;; (format #t "Connections active: ~a\n"
		;; 	(length connection-threads))
		))))
	(lambda () 			; dynamic wind post_guard
	  (close sock)
	  (unless (zero? child-pid)
	    (delete-file sockpath))))))
  ;; catch handler
  (lambda (key . args)
    (cond
      ((eq? key 'SIGINT) (display "Exit on keyboard interrupt.\n"))
      (else
       (format #t "~a: ~a\n" key args)
       ;; (throw key args)
       ))))
