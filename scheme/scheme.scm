(provide compile #%compile)

(require-builtin freedom/scheme)

(define-syntax compile
  (syntax-rules []
    [(_ . body) (#%compile (quote . body))]))
