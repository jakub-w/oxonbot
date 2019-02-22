;; NOTES:
;; Every client connecting to this server should have a unique id that cannot
;; be easily spoofed. It will use this id to confirm its identity.
;; Everything that the client does will be interpreted with this id in mind.
;; The client will be responsible for generating and providing that id.
;; It will be used as part of the context in the database.
;; Context will be: client_id, path (like server:#channel), caller (username)


(use-modules (ice-9 rdelim)
	     (ice-9 threads)
	     (srfi srfi-1)
	     (rnrs io ports))

(define connection-threads '())

(sigaction SIGINT
  (lambda (x)
    (for-each (lambda (thread)
		; FIXME: this could throw and join-thread would hang
		;        indefinitely
		(cancel-thread thread))
	      connection-threads)
    (display "Cancelling the connection threads...\n")
    (for-each (lambda (thread)
		(join-thread thread))
	      connection-threads)
    (display "All connections were closed.\n")
    (throw 'SIGINT x)))

(define (connection-handler client-connection)
  "CLIENT-CONNECTION should be a pair returned from the `accept' function."
  (unless (and (pair? client-connection)
	       (port? (car client-connection)))
    (throw 'bad-client-connection 'connection-handler
	   "client-connection is not a valid connection"))
  (let ((client (car client-connection))
	(client-details (cdr client-connection)))
    (dynamic-wind
      (lambda () #f)
      (lambda ()
	;; event loop to listen for requests from the client
	(let ((exit? #f))
	  (while (not exit?)
	    (let ((line (read-line client)))
	      (when (or (and (string? line)
	    		     (string=? (string-trim-right line) "exit"))
	    		(eof-object? line))
	    	(set! exit? #t))
	      (monitor (display line)
	    	       (newline))))))
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

	  (let ((new-connection #f)
		(exit? #f))
	    (while (not exit?)
	      (set! new-connection (accept sock))
	      (if new-connection
  		  (begin
		    (format #t "New connection: ~a\n" new-connection)
		    (set! connection-threads
		      (cons (begin-thread
			     (connection-handler new-connection))
			    connection-threads)))
  		  (sleep 1)))))
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
