(provide load-script unload-script get-script)

(require "../core.scm")
(require "../async.scm")
(require "../fs/mod.scm")

(require-builtin freedom/log)

(define *scripts* (hash))

(define scripts-dir (make-parameter "scheme"))

(define [script-path name]
  (define scripts-dir (scripts-dir))
  (define name (symbol->string name))
  (string-append scripts-dir "/" name ".scm"))

(define [%load-script path]
  (info! "Loading" path "...")
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

(define [path-relevant? path]
  (define path (normalize-path path))
  (hash-contains? *scripts* path))

(define [apply-change change]
  (define path (car change))
  (define path (normalize-path path))
  (define val (cadr change))
  (match val
    ['reload (%load-script path)]
    ['unload (%unload-script path)]
    [else void]))

(spawn
  (lambda ()
    (watch-loop (scripts-dir) #t path-relevant? apply-change)))