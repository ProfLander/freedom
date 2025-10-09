(provide yield await)

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

(let ([cc (current-continuation)])
  (if (continuation? cc)
    (set! halt (lambda () (cc void)))
    (displayln "halting")))