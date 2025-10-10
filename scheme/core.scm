(provide compile)

(define-syntax compile
  (syntax-rules []
    [(compile . body) (#%compile (quote . body))]))

(define-syntax #%require-plugin
  (syntax-rules [only-in]
    [(_ plugin-name (only-in name ...))
     (begin
       (define name (%module-get% (#%get-plugin (quote plugin-name)) (quote name)))
       ...)]))