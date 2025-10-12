(require "core.scm")
(require "async.scm")
(require "scripts.scm")

(#%require-plugin library (only-in print-hello))

(print-hello)

(spawn (get-script 'print-loop))