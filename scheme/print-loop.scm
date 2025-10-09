(provide print-loop)

(require-builtin freedom/async)
(require-builtin library)

(define [print-loop]
  (displayln "Print loop")
  (print-hello)
  (#%spawn print-loop))
