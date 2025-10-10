(require "async.scm")
(require "core.scm")

(#%require-plugin library (only-in print-hello))

(displayln "Print loop")
(print-hello)
(spawn 'print-loop)