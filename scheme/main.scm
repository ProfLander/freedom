(require-builtin freedom/async)
(require-builtin freedom/winit)
(require-builtin library)

(require "repl.scm")
(require "print-loop.scm")

(define device-event
  (#%spawn (#%compile '(displayln "device event"))))

(#%spawn
  (#%winit #%executor
    #:device-event device-event))

(#%spawn repl)
(#%spawn print-loop)