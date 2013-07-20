scheme-logger
=============

This is a very simple logging facility written in Scheme packaged as a chicken egg.

Features are:
1. support 4 levels of verbosity levels (error, warning, info, debug)
2. redirect output to a file rather than stdout
3. allow replacement of the logger function for custom formatting

The default behavior is to log everything to the console in this format:
[date/time] [verbosity level] arg1 arg2 ...

