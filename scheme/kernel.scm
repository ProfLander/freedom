(require-builtin freedom/log)
(info! "Starting kernel ...")

(require "scheme.scm")
(require "async.scm")
(require "parallel.scm")

(require "plugins/mod.scm")
(require "scripts/mod.scm")

(require "fs/watcher.scm")
; FIXME: Dropping active watcher causes a MAV
;(require "plugins/watcher.scm")
;(require "scripts/watcher.scm")

(define [*on-exit*]
  (info! "Exiting...")
  (stop-watcher))