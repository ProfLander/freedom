(require "core.scm")
(require "async.scm")

(#%require-plugin library (only-in print-hello))

(print-hello)

(spawn (script print-loop))