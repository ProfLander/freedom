(require-builtin freedom/async)
(require-builtin freedom/winit)
(require-builtin library)

(#%spawn (run-winit #%executor))

(define [loop]
  (displayln "Print loop")
  (print-hello)
  (#%spawn loop))
(#%spawn loop)

(#%spawn 'repl)
