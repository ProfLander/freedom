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
    
    (require-builtin freedom/log)

    (define [*resumed* el]
      (info! "resumed"))

    (define [*suspended* el]
      (info! "suspended"))

    (define [*new-events* el cause]
      (info! "new events:" cause))

    (define [*device-event* el dev ev]
      (info! dev ev))

    (define [*window-event* el win ev]
      (info! win ev))

    (define [*about-to-wait* el]
      (info! "about to wait"))

    (define [*exiting* el]
      (info! "exiting"))

    (define [*memory-warning* el]
      (info! "memory warning"))))

(info! "Starting winit...")
(spawn
  (#%winit-run *worker-id* #%scheme-config #%executor init))