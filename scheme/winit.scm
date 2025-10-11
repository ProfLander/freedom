(require "core.scm")
(require "async.scm")

(require-builtin freedom/log)
(require-builtin freedom/async)

(#%require-plugin freedom_winit (only-in #%winit-run))

(define-syntax winit-callback
  (syntax-rules []
    [(_ args . body)
     '(lambda args . body)]))

(define-syntax program
  (syntax-rules []
    [(_ . body)
     (quote (list . body))]))

(define init
  (program
    (require "scheme/async.scm")

    (define [*resumed* el]
      (displayln "resumed:" el))

    (define [*suspended* el]
      (displayln "suspended:" el))

    (define [*new-events* el cause]
      (displayln "new events:" el cause))

    (define [*device-event* el dev ev]
      (displayln "device event:" el dev ev))

    (define [*window-event* el win ev]
      (displayln "window event:" el win ev))

    (define [*about-to-wait* el]
      (displayln "about to wait:" el))

    (define [*exiting* el]
      (displayln "exiting:" el))

    (define [*memory-warning* el]
      (displayln "memory warning:" el))))

(info! "Starting winit...")
(spawn
  (#%winit-run #%executor init))