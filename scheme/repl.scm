(display "]> ")
(call-with-exception-handler
  (lambda (e)
    (displayln "ERROR:" e))
  (lambda ()
    (displayln (eval-string (read-line)))))
(#%spawn 'repl)