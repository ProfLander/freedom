(require "async.scm")
(require "plugins/mod.scm")
(require "scripts/mod.scm")

(#%require-plugin library (only-in print-hello))

(print-hello)

(spawn (get-script 'print-loop))