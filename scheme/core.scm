(provide compile #%compile)

(require-builtin freedom/scheme)

(require "plugins/mod.scm")

(define-syntax compile
  (syntax-rules []
    [(_ . body) (#%compile (quote . body))]))
