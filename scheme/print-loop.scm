(require "core.scm")
(require "async.scm")

(#%require-plugin library (only-in print-hello))

(displayln "Print loop")
(print-hello)

(spawn (script print-loop))