(provide yield await spawn #%executor)

(require-builtin freedom/async)

(define [current-continuation] (call/cc (lambda (cc) (cc cc))))

(define halt #f)

(define [yield]
  (let ([cc (current-continuation)])
    (when (continuation? cc)
      (#%spawn (lambda () (cc void)))
      (halt))))

(define [await fut]
  (let ([cc (current-continuation)])
    (if (continuation? cc)
      (begin
        (#%await fut cc)
        (halt))
      cc)))

(define-syntax spawn
  (syntax-rules []
    [(spawn exp ...) (begin (#%spawn exp) ...)]))

(let ([cc (current-continuation)])
  (when (continuation? cc)
    (set! halt (lambda () (cc void)))))