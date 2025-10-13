(require-builtin freedom/log)
(info! "Starting kernel on thread" (current-thread-id) "...")

(require "scheme.scm")
(require "async.scm")

(require "plugins/mod.scm")
(require "scripts/mod.scm")

(require "plugins/watcher.scm")
(require "scripts/watcher.scm")
