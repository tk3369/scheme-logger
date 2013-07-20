;;
;; A simple logging facility
;;
;; Sample usage:
;; (use logger)
;; (log-set-verbosity! 'debug) ;; any of '(error warning info debug)
;;
;; @author Tom Kwong
;; @copyright 2013 All rights reserved
;;

(module logger 
	(log-debug log-info log-warning log-error
	 log-get-verbosity log-set-verbosity!
	 log-set-output-file!
	 log-reset-output-port!)
	
(import scheme chicken)
(use srfi-1)
(use posix)

;; default values
(define log-default-verbosity-level 2) ; 0=error 1=warning 2=info 3=debug
(define log-default-output-port ##sys#standard-output)

;; global variables
(define log-verbosity-level log-default-verbosity-level)
(define log-output-port log-default-output-port)

;; this private function requires posix
(define (%current-datetime-string)
  (time->string (seconds->local-time (current-seconds)) "%Y-%m-%d %H:%M:%S"))

;; private function that logs data into the output port
;; TODO need to make this function thread safe?
;; TODO add buffer size to determine when to flush?
(define (%log-function . args)
  (for-each (lambda (x) (display x log-output-port))
	    (append (list (%current-datetime-string) " ") args '(#\newline)))
  (flush-output log-output-port)
  #t)

(define (log-debug . args) 
  (if (> log-verbosity-level 2)
      (apply %log-function (cons "DEBUG: " args))
      #f))

(define (log-info . args) 
  (if (> log-verbosity-level 1)
      (apply %log-function (cons "INFO: " args))
      #f))

(define (log-warning . args)
  (if (> log-verbosity-level 0)
      (apply %log-function (cons "WARNING: " args))
      #f))

(define (log-error . args) 
  (apply %log-function (cons "ERROR: " args)))

(define (log-get-verbosity)
  log-verbosity-level)

(define (log-set-verbosity! v)
  (let* ((sym '(error warning info debug))
	 (idx (list-index eq? sym (circular-list v))))
    (if idx
	(set! log-verbosity-level idx)
	(log-error "invalid verbosity label: " v "; it must be one of '(" sym ")"))))

(define (log-set-output-file! filepath)
  (call-with-current-continuation
   (lambda (k)
     (with-exception-handler
      (lambda (e)
        (print "ERROR: cannot open output file " filepath)
        (k e))
      (lambda ()
	;; close previous port 
	(if (not (eq? log-output-port log-default-output-port))
	    (close-output-port log-output-port))
	;; open new output port
	(let ((p (open-output-file filepath #:append)))
	  (print "logger opened output file " filepath)
	  (set! log-output-port p))))))) 

(define (log-reset-output-port!)
  (set! log-output-port log-default-output-port))

) ; end module
