#!/usr/bin/guile
!#
;; Dependencies:
;; external: sqlite3
;; guile: guile-sqlite3 (https://notabug.org/guile-sqlite3/guile-sqlite3.git)

;; Notes:
;; 'user-error is a type of error that will be shown to the user


(add-to-load-path "guile-sqlite3/build/")

(use-modules (ice-9 regex)
	     (ice-9 textual-ports)
	     (srfi srfi-1)
	     (srfi srfi-19)
	     (sqlite3))


;; UTILITY FUNCTIONS
;; (define (get-previous-line port)
;;   (let ((pos (ftell port))
;; 	(line '(#\newline))
;; 	(char (lookahead-char port)))
;;     (if (eq? pos 0)
;; 	#f ; return false if beginning of the file
;; 	(begin
;; 	  (when (or (eof-object? char); skip eof obj if sitting on it
;; 		    (eq? char #\newline)) ; line is newline to newline
;; 	    (seek port -1 SEEK_CUR))	  ; so skip one if beggining with it
;; 	  (while #t
;; 	    (set! char (lookahead-char port))
;; 	    (when (eq? char #\newline)
;; 	      (break)) ; do not include newline char at the beginning of line
;; 	    (set! line (cons char line))
;; 	    (when (eq? pos 0)
;; 	      (break))
;; 	    (set! pos (seek port -1 SEEK_CUR)))
;; 	  (list->string line)))))

;; logging
(define (current-time-utc)
  (date->string (current-date 0)
		"~Y-~m-~dT~H:~M:~SZ"))

(define (error-log text)
  "Add error to a log file. Use UTC time."
  (let ((port (open-file "error.log" "a")))
    (put-string port (format #f "~a\t~a\n"
			     (current-time-utc)
			     text))
    (close-port port)))
;; end of logging
;; END OF UTILITY FUNCTIONS


;; INTERNAL FUNCTIONS TO THOSE EXPOSED TO THE COMMAND LINE

;; roll command
(define (roll-internal . args)
  (let* ((limits (map (lambda (m)
			(string->number (match:substring m)))
		      (list-matches "[0-9]+" (car args))))
	 (limits-len (length limits))
	 (repetitions (if (null? (cdr args))
			  1
			  (or (string->number (cadr args)) 1))))
    ;; return false if the second value is smaller than the first (roll 20-10)
    ;; if repetitions are less than 1 return false too
    (if (or (zero? limits-len)
	    (and (> limits-len 1)
    		 (> (car limits) (cadr limits)))
    	    (< repetitions 1))
    	#f
    	(do ((i 0 (1+ i))
	     (rand-state (random-state-from-platform))
    	     (roll-list '()
    			(cons
    			 (case limits-len
    			   ((1) (1+ (random (car limits) rand-state)))
    			   ;; e.g. roll 3-6: 6-3=3 -> 3+1=4 ->
    			   ;;      rand(4)=<0;3> -> 0+3=3, 3+3=6
    			   ((2) (+ (car limits)
    				   (random (1+ (- (cadr limits)
    						  (car limits)))
					   rand-state))))
    			 roll-list)))
    	    ((>= i repetitions)
    	     (cons roll-list (reduce + 0 roll-list)))))))
;; end of roll command

;; quote command
;; TODO: add context column and author to quotes table
;;       context would be an irc channel or discord channel (maybe in a form
;;       of irc(freenode):#channel, meaning protocol(server):channel)
;; TODO: create ensure-database function that will create database and all of
;;       the tables if they don't exist

(define db-name "quotes.db")

;; can throw 'sqlite-error
(define (quote-add text)
  (let ((db (sqlite-open db-name (logior SQLITE_OPEN_CREATE
                                         SQLITE_OPEN_READWRITE))))
    (when (sqlite-db? db)
      (sqlite-exec db "
CREATE TABLE IF NOT EXISTS quotes
(id INTEGER PRIMARY KEY AUTOINCREMENT, date TEXT, quote TEXT)")
      (let ((stmt (sqlite-prepare db "
INSERT INTO quotes(date, quote) VALUES(?,?)")))
	(sqlite-bind-arguments
	 stmt
	 (date->string (current-date) "~d.~m.~Y ~H:~M")
	 text)
	(sqlite-step stmt)
	(sqlite-finalize stmt)))
    (sqlite-close db)))

;; TODO: do something with this (throw 'user-error ...) since this was moved
;;       to more internal function
(define* (quote-get-from-db sql . bind-arguments)
  "Get result of a query to the quote database.
The database is opened in read only mode.

SQL is an sql query.

BIND-ARGUMENTS is an optional list of arguments to bind if SQL is a prepared
               statement."
  (catch 'sqlite-error
    (lambda ()
      (let* ((db (sqlite-open db-name SQLITE_OPEN_READONLY))
	     (stmt (sqlite-prepare
		    db sql))
	     (result #f))
	(if (null? bind-arguments)
	    (set! result (sqlite-map identity stmt))
	    (begin
	      (apply sqlite-bind-arguments (cons stmt bind-arguments))
	      (set! result (sqlite-step stmt))
	      (sqlite-finalize stmt)))
	(sqlite-close db)
	(when (or (null? result)
		  (not result))
	    (throw 'user-error "Quote not found."))
	result))
    (lambda (key . args)
      ;; Since db is opened as readonly the missing table cannot be recreated
      ;; so the user is notified that there are no quotes in the database.
      ;; If the problem is not comming from sqlite-prepare, rethrow it.
      (if (and (eq? (car args) 'sqlite-prepare)
	       (eq? (cadr args) 1)) ; generic SQL_ERROR
	  (begin
	    (throw 'user-error "No quotes in the database.")
	    (error-log (format #f "~s: ~s" key args))) ; log it regardless
	  (throw key args)))))

(define (quote-random)
  ;; this returns a list, so car is used to get the entry
  (car (quote-get-from-db "SELECT * FROM quotes ORDER BY random() LIMIT 1")))

(define (quote-read id)
  (if (equal? id "last")
      ;; this returns a list, so car is used to get the entry
      (car (quote-get-from-db
	    "SELECT * FROM quotes ORDER BY id DESC LIMIT 1"))
      ;; this returns a singular entry
      (quote-get-from-db
       "SELECT * FROM quotes WHERE id = ?" id)))

(define (format-quote quote)
  (format #f "Quote #~a added ~a by ~a:\n~a\n"
	  (car quote) (cadr quote) "anonymous" (caddr quote)))

;; end of quote command

;; END OF INTERNAL FUNCTIONS


;; FUNCTIONS EXPOSED TO THE COMMAND LINE

(define (roll-command args)
  "ARGS have to be <upper>|<lower>-<upper> [repetitions]
The result of a roll will be in [1, <upper>] or [<lower>, <upper>].

Return a pair consisting of a list with rolls and the sum of them.

Examples:

roll 10 : random number from 1 to 10
roll 10 2 : two random numbers from 1 to 10 (and than summed)
roll 10-100 : random number from 10 to 100
roll 10-100 3 : three random numbers from 10 to 100 (and than summed)"
  (if (< (length args) 1)
      (display "Usage: roll <upper>|<lower>-<upper> [repetitions]\n")
      (let ((result (apply roll-internal args)))
	(when result
	  (for-each (lambda (roll-result)
		      (format #t "~s, " roll-result))
		    (car result))
	  (format #t "SUM: ~s\n" (cdr result)))
	result)))

;; TODO: check fo
(define (quote-command args)
  (catch 'sqlite-error
    (lambda ()
      (if (> (length args) 0)
	  (cond
	   ((equal? (car args) "add")
      	    (if (< (length args) 2)
      		(display "Usage: quote add <text>\n")
      		(begin
		  (apply quote-add (list (string-join (cdr args))))
		  (display "Quote added successfully.\n"))))
	   ((equal? (car args) "random")
	    (display (format-quote (vector->list (quote-random)))))
	   ((equal? (car args) "read")
	    (if (< (length args) 2)
		(display "Usage: quote read <id>|last\n")
		(display (format-quote (vector->list
					(quote-read (cadr args)))))))
	   (else (format #t "Subcommand '~a' not found.\n" (car args))))
	  (display "Subcommands available: add, random, read\n")))
    (lambda (key . args)
      (error-log (format #f "~a: ~a" key args))
      (display "Database problem. Operation failed...\n"))))

;; END OF EXPOSED FUNCTIONS

(define named-functions
  (list (cons "roll" roll-command)
	(cons "quote" quote-command)))

(define (name->function name)
  (let ((fun (assoc name named-functions)))
    (if fun
	(cdr fun)
	(throw 'user-error
	       (format #f "Command '~a' not found." name)))))

(define (handle-command-line)
  (let ((args (cdr (command-line))))
    (if (null? args)
	(format #t "Commands available:~a\n"
		(substring/shared
		 (fold (lambda (fun-obj prev)
			 (string-append prev ", " (car fun-obj)))
		       ""
		       named-functions)
		 1))
	(catch #t
	  (lambda ()
	    (apply (name->function (car args)) (list (cdr args))))
	  (lambda (key . args)
	    (if (eq? key 'user-error)
		(format #t "~a\n" (car args))
		(begin
		  (display "Something went wrong...\n")
		  (error-log (format #f "~a: ~a" key args)))))))))

(handle-command-line)