(provide script compile #%require-plugin)

(require-builtin freedom/scheme)
(require-builtin freedom/scripts)
(require-builtin freedom/plugins)

(define-syntax script
  (syntax-rules []
    [(_ name) (await (#%get-script (quote name)))]))

(define-syntax compile
  (syntax-rules []
    [(compile . body) (#%compile (quote . body))]))

(define-syntax #%require-plugin
  (syntax-rules [only-in]
    [(_ plugin-name (only-in name ...))
     (begin
       (define name (%module-get% (#%get-plugin (quote plugin-name)) (quote name)))
       ...)]))