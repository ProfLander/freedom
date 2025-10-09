(require-builtin freedom/async)
(require-builtin freedom/winit)

(define device-event
  (#%spawn (#%compile '(displayln "device event"))))

(#%spawn
  (#%winit #%executor
    #:device-event device-event))