(require-builtin freedom/async)
(require-builtin freedom/winit)

(require "core.scm")
(require "async.scm")

(displayln "compiling winit callbacks")
(let ([resumed (await (compile (displayln "resumed")))]
      [suspended (await (compile (displayln "suspended")))]
      [new-events (await (compile (displayln "new events")))]
      [device-event (await (compile (displayln "device event")))]
      [window-event (await (compile (displayln "window event")))]
      [about-to-wait (await (compile (displayln "about to wait")))]
      [exiting (await (compile (displayln "exiting")))]
      [memory-warning (await (compile (displayln "memory warning")))])
      
  (displayln "starting winit:")
  (displayln resumed)
  (displayln suspended)
  (displayln new-events)
  (displayln device-event)
  (displayln window-event)
  (displayln about-to-wait)
  (displayln exiting)
  (displayln memory-warning)

  (#%spawn
    (#%winit #%executor
      #:resumed resumed
      #:suspended suspended
      ;#:new-events new-events
      #:device-event device-event
      #:window-event window-event
      ;#:about-to-wait about-to-wait
      #:exiting exiting
      #:memory-warning memory-warning)))