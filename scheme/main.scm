(require "core.scm")
(require "async.scm")

(spawn
  (script print-loop)
  (script repl)
  (script winit))