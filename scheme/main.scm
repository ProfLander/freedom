(info! "main")

;(define print-loop (Thread #%scheme-config "scheme/print-loop.scm"))

(info! "Spawning REPL...")
(define channels (make-channels))
(define tx (car channels))
(define rx (cadr channels))

;(define repl
;  (thread-spawn
;    (lambda ()
;      (define *repl-tx* tx)
;      (load "scheme/repl.scm"))
;    #:name 'REPL))

(info! "Spawning async tasks...")
(async
  (spawn
    ;(get-script 'print-loop)
    ;(get-script 'cpal)
    (get-script 'winit))
  ;(Thread-join print-loop)
  ;(Thread-join repl)
  )
