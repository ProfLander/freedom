(require "core.scm")
(require "async.scm")

(require-builtin freedom/log)
(require-builtin freedom/async)

(#%require-plugin freedom_winit (only-in #%winit))

(define-syntax winit-callback
  (syntax-rules []
    [(_ args . body)
     '(lambda args . body)]))

(define resumed
  (winit-callback (el)
    (displayln "resumed:" el)))

(define suspended
  (winit-callback (el)
    (displayln "suspended:" el)))

(define new-events
  (winit-callback (el cause)
    (displayln "new events:" el cause)))

(define device-event
  (winit-callback (el dev ev)
    (displayln "device event:" el dev ev)))

(define window-event
  (winit-callback (el win ev)
    (displayln "window event:" el win ev)))

(define about-to-wait
  (winit-callback (el)
    (displayln "about to wait:" el)))

(define exiting
  (winit-callback (el)
    (displayln "exiting:" el)))

(define memory-warning
  (winit-callback (el)
    (displayln "memory warning:" el)))
      
(info! "Starting winit...")
(spawn
  (#%winit #%executor
    #:resumed resumed
    #:suspended suspended
    #:new-events new-events
    #:device-event device-event
    #:window-event window-event
    ;#:about-to-wait about-to-wait
    #:exiting exiting
    #:memory-warning memory-warning))