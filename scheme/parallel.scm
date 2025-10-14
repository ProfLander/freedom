(provide thread-spawn thread-join)

(require-builtin freedom/parallel)

(define [thread-spawn f . args]
  (define channels (make-channels))
  (define tx (car channels))
  (apply Thread (list #%scheme-config tx . args))
  (define rx (cadr channels))
  (define tx (channel->recv rx))
  (channel->send tx f))

(define [thread-join thread]
  (Thread-join thread))
