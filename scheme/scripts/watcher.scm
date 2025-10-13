(require "mod.scm")
(require "../fs/mod.scm")

(define [path-relevant? path]
  (define path (normalize-path path))
  (hash-contains? (scripts) path))

(define [apply-change change]
  (info! "apply-change" change)
  (define path (car change))
  (define path (normalize-path path))
  (define val (cadr change))
  (match val
    ['reload (%load-script path)]
    ['unload (%unload-script path)]
    [else void]))

(async
  (spawn
    (lambda ()
      (define scripts-dir (scripts-dir))
      (info! "Starting scripts watch loop at" scripts-dir)
      (watch-loop scripts-dir #t path-relevant? apply-change))))