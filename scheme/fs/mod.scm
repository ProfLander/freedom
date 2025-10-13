(provide delete-file! copy-file! temp-dir unique-id normalize-path)

(require-builtin freedom/fs)

(define [normalize-path path]
  (define path (trim-start-matches path (current-directory)))
  (define path (string-replace path "\\" "/"))
  (define path (trim-start-matches path "/"))
  path)
