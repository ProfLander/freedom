;(provide *scripts*)

(require "core.scm")
(require "async.scm")

(require-builtin freedom/fs)

(define *scripts* (hash))

(define [load-script name]
  (let* ([path (string-append "scheme/" name ".scm")]
         [port (open-input-file path)]
         [data (read-port-to-string port)])
    (set! *scripts* (hash-insert *scripts* name (await (compile (read data)))))
    (displayln *scripts*)))

(load-script "main")

(define watcher (Watcher 0.1 #f))
(displayln watcher)

(Watcher-watch watcher "scheme" '(RecursiveMode (NonRecursive)))

(while #t
  (let ([events (await (Watcher-events watcher))])
    (displayln "Watcher events:" events)))