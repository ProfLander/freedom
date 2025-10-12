(require "core.scm")
(require "async.scm")
(require "file-watcher.scm")

(spawn
  (script print-loop)
  (script repl)
  (script winit))