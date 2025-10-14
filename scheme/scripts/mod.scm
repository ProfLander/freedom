(provide
  scripts-dir scripts
  %load-script load-script
  %unload-script unload-script
  get-script)

(require-builtin freedom/log)

(define scripts-dir (make-parameter "scheme"))

(define *scripts* (hash))

(define [scripts]
  *scripts*)

(define [script-path name]
  (define scripts-dir (scripts-dir))
  (define name (symbol->string name))
  (string-append scripts-dir "/" name ".scm"))

(define [%load-script path]
  (info! "Loading script from" path "...")
  (define port (open-input-file path))
  (define src (read-port-to-string port))
  (define script (await (#%compile src path)))
  (set! *scripts* (hash-insert *scripts* path script)))

(define [load-script name]
  (%load-script (script-path name)))

(define [%unload-script path]
  (info! "Unloading" path "...")
  (set! *scripts* (hash-remove *scripts* path)))

(define [unload-script name]
  (%unload-script (script-path name)))

(define [get-script name]
  (define path (script-path name))
  (when (not (hash-try-get *scripts* path))
    (%load-script path))
  (hash-get *scripts* path))