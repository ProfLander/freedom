(require-builtin freedom/log)

(call-with-exception-handler
  (lambda (e)
    (error! "ERROR:" e))
  (lambda ()
    (displayln "*repl-tx*:" *repl-tx*)
    (display "> ")
    (flush-output-port (current-output-port))
    (displayln (eval-string (read-line)))))

(async (spawn (get-script 'repl)))