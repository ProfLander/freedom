(require "async.scm")
(require "scripts/mod.scm")

(require-builtin freedom/log)

(call-with-exception-handler
  (lambda (e)
    (error! "ERROR:" e))
  (lambda ()
    (display "> ")
    (flush-output-port (current-output-port))
    (displayln (eval-string (read-line)))))

(spawn (get-script 'repl))
