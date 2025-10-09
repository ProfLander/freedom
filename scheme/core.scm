(provide compile)

(define-syntax compile
  (syntax-rules []
    [(compile . body) (#%compile (quote . body))]))