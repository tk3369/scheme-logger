scheme-logger
=============

This is a very simple logging facility written in Scheme packaged as a chicken egg.

Features are:
- support 4 levels of verbosity levels (error, warning, info, debug)
- redirect output to a file rather than stdout
- allow replacement of the logger function for custom formatting

The default behavior is to log everything to the console in this format:
```
[date/time] [verbosity level] arg1 arg2 ...
```

## usage

``` scheme
(import logger)
; set your logging level to one of the following
; 'debug 'info 'warning 'error
(log-set-verbosity! 'testing)
; optionally log to a file
(log-set-output-file! "path/to/log_file.log")
; log a message
(log-debug "Here's a debugging message.")
(log-info "Just an FYI: That thing happened.")
(log-warning "Something bad happened, but i handled it.")
(log-error "Something bad happened and I don't feel good.")
```
