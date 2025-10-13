(provide normalize-path)

(define [normalize-path path]
  (define path (trim-start-matches path (current-directory)))
  (define path (string-replace path "\\" "/"))
  (define path (trim-start-matches path "/"))
  path)
