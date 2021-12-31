;;
;; A simple logging facility
;;
;; Sample usage:
;;
;; (import logger)
;; ; set your logging level to one of the following
;; ; 'debug 'info 'warning 'error
;; (log-set-verbosity! 'testing)
;; ; optionally log to a file
;; (log-set-output-file! "path/to/log_file.log")
;; ; log a message
;; (log-debug "Here's a debugging message.")
;; (log-info "Just an FYI: That thing happened.")
;; (log-warning "Something bad happened, but i handled it.")
;; (log-error "Something bad happened and I don't feel good.")
;; @author Tom Kwong
;; @copyright 2013 All rights reserved
;;
;; Updated to Chicken 5.x Dec 2021 by masukomi

(module logger 
	(log-debug log-info log-warning log-error
	 log-get-verbosity log-set-verbosity!
	 log-set-logger! 
	 log-set-output-file!
	 log-reset-output-port!)

  (import chicken.base)
  (import chicken.condition)
  (import chicken.format)
  (import chicken.time)
  (import chicken.time.posix)
  (import scheme)
  (import srfi-1)

;; this private function requires posix
(define (%current-datetime-string)
  (time->string (seconds->local-time (current-seconds)) "%Y-%m-%d %H:%M:%S"))

;; private function that logs data into the output port
(define (%default-logger . args)
  (for-each (lambda (x) 
	      (display x output-port)
	      (display " " output-port))
	    (append 
	     (list (%current-datetime-string))
	     args))
  (display #\newline output-port)
  (flush-output output-port)
  #t)

;; default values
(define default-verbosity-level 2) ; 0=error 1=warning 2=info 3=debug
(define default-output-port ##sys#standard-output)

;; global variables
(define verbosity-level default-verbosity-level)
(define output-port default-output-port)
(define logger %default-logger)

(define (log-debug . args) 
  (if (> verbosity-level 2)
      (apply logger (cons "DEBUG  " args))
      #f))

(define (log-info . args) 
  (if (> verbosity-level 1)
      (apply logger (cons "INFO   " args))
      #f))

(define (log-warning . args)
  (if (> verbosity-level 0)
      (apply logger (cons "WARNING" args))
      #f))

(define (log-error . args) 
  (apply logger (cons "ERROR  " args)))

(define (log-get-verbosity)
  verbosity-level)

(define (log-set-verbosity! v)
  (let* ((sym '(error warning info debug))
	 (idx (list-index eq? sym (circular-list v))))
    (if idx
	(set! verbosity-level idx)
	(log-error "invalid verbosity label: " v "; it must be one of '(" sym ")"))))

(define (log-set-logger! f)
  (set! logger f))

(define (log-set-output-file! filepath)
  (call-with-current-continuation
   (lambda (k)
     (with-exception-handler
      (lambda (e)
        (print "ERROR: cannot open output file " filepath)
        (k e))
      (lambda ()
	;; close previous port 
	(if (not (eq? output-port default-output-port))
	    (close-output-port output-port))
	;; open new output port
	(let ((p (open-output-file filepath #:append)))
	  (print "logger opened output file " filepath)
	  (set! output-port p))))))) 

(define (log-reset-output-port!)
  (set! output-port default-output-port))

) ; end module
