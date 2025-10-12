(require "async.scm")
(require "scripts.scm")

(spawn
  (get-script 'print-loop)
  (get-script 'repl)
  (get-script 'winit))