(require "mod.scm")
(require "../fs/mod.scm")
(require "../fs/watcher.scm")

(define [path-relevant? path]
  (define path (normalize-path path))
  (hash-contains? (plugins) path))

(define [apply-change change]
  (define path (car change))
  (define path (normalize-path path))
  (define val (cadr change))
  (match val
    ['reload (%load-plugin path)]
    [else void]))

(spawn
  (lambda ()
    (watch-loop (plugins-dir) #t path-relevant? apply-change)))