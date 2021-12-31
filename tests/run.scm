(import srfi-78)
(check-set-mode! 'summary)

(import logger)

;; test output to console
(log-set-verbosity! 'debug)
(check (log-debug 'testing) => #t) 
(check (log-info 'testing) => #t) 
(check (log-warning 'testing) => #t) 
(check (log-error 'testing) => #t) 

(log-set-verbosity! 'info)
(check (log-debug 'testing) => #f) 
(check (log-info 'testing) => #t) 
(check (log-warning 'testing) => #t) 
(check (log-error 'testing) => #t)

(log-set-verbosity! 'warning)
(check (log-debug 'testing) => #f) 
(check (log-info 'testing) => #f) 
(check (log-warning 'testing) => #t) 
(check (log-error 'testing) => #t) 
 
(log-set-verbosity! 'error)
(check (log-debug 'testing) => #f) 
(check (log-info 'testing) => #f) 
(check (log-warning 'testing) => #f) 
(check (log-error 'testing) => #t) 

;; test ouptut to file
(log-set-output-file! "/tmp/logger-unit-test-1.log")

(log-set-verbosity! 'debug)
(check (log-debug 'testing) => #t) 
(check (log-info 'testing) => #t) 
(check (log-warning 'testing) => #t) 
(check (log-error 'testing) => #t) 

(log-set-verbosity! 'info)
(check (log-debug 'testing) => #f) 
(check (log-info 'testing) => #t) 
(check (log-warning 'testing) => #t) 
(check (log-error 'testing) => #t)

(log-set-verbosity! 'warning)
(check (log-debug 'testing) => #f) 
(check (log-info 'testing) => #f) 
(check (log-warning 'testing) => #t) 
(check (log-error 'testing) => #t) 
 
(log-set-verbosity! 'error)
(check (log-debug 'testing) => #f) 
(check (log-info 'testing) => #f) 
(check (log-warning 'testing) => #f) 
(check (log-error 'testing) => #t) 

;; switch to another output file
(log-set-output-file! "/tmp/logger-unit-test-2.log")
(log-set-verbosity! 'debug)
(check (log-debug 'testing) => #t) 
(check (log-info 'testing) => #t) 
(check (log-warning 'testing) => #t) 
(check (log-error 'testing) => #t) 


(if (check-passed? 36)
    (exit 0)
    (exit 1))
