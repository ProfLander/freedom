(require "async.scm")
(require "plugins/mod.scm")
(require "scripts/mod.scm")

(spawn
  (get-script 'print-loop)
  (get-script 'repl)
  (get-script 'winit))