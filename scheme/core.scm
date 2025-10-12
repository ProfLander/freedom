(provide compile #%compile #%require-plugin)

(require-builtin freedom/scheme)
(require-builtin freedom/plugins)

(define-syntax compile
  (syntax-rules []
    [(_ . body) (#%compile (quote . body))]))

(define-syntax #%require-plugin
  (syntax-rules [only-in]
    [(_ plugin-name (only-in name ...))
     (begin
       (define name
         (or
           (%#maybe-module-get (#%get-plugin (quote plugin-name)) (quote name))
           (error "No symbol" (quote name) "in" (quote plugin-name))))
       ...)]))