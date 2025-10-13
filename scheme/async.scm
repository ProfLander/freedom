(provide async yield await spawn #%executor)

(require-builtin freedom/async)

(define [current-continuation] (call/cc (lambda (cc) (cc cc))))

;; Halt continuation
(define halt #f)

;; Entrypoint, to be called once at the top level
(define [%async cont]
  (let ([cc (current-continuation)])
    (when (continuation? cc)
      (set! halt (lambda () (cc void)))
      (cont))))

;; Entrypoint with automatic thunk, to be called once at the top level
(define-syntax async
  (syntax-rules []
    [(_ . body) (%async (lambda () . body))]))

;; Spawn the current continuation as a task and halt the scheme machine
(define [yield]
  (let ([cc (current-continuation)])
    (when (continuation? cc)
      (#%spawn (lambda () (cc void)))
      (halt))))

;; Await FUT, and call the current continuation with its result
(define [await fut]
  (let ([cc (current-continuation)])
    (if (continuation? cc)
      (begin
        (#%await fut cc)
        (halt))
      cc)))

;; Spawn a task onto the async executor
(define-syntax spawn
  (syntax-rules []
    [(spawn exp ...) (begin (#%spawn exp) ...)]))
