(require-builtin freedom/log)
(info! "Starting kernel for worker" *worker-id* "...")

(require "scheme.scm")
(require "async.scm")

(require "plugins/mod.scm")
(require "scripts/mod.scm")

(require "fs/watcher.scm")
;(require "plugins/watcher.scm")
;(require "scripts/watcher.scm")

(define [*on-exit*]
  (info! "Worker" *worker-id* "exiting")
  (stop-watcher))