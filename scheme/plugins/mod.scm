(provide load-plugin unload-plugin get-plugin #%require-plugin)

(require-builtin freedom/log)
(require-builtin freedom/loading)

(require "../async.scm")
(require "../fs/mod.scm")
(require "plugin.scm")

(define *plugins* (hash))

;; Search directory parameter
(define plugins-dir (make-parameter "target/debug/deps"))

;; Given a plugin name, return a file path
(define [plugin-path name]
  (string-append (plugins-dir) "/" (#%library-filename name)))

(define [%load-plugin path]
  (define plugin (#%load-plugin path))
  (set! *plugins* (hash-insert *plugins* path plugin))
  (info! "plugins:" *plugins*))

(define [load-plugin name]
  (define path (plugin-path name))
  (%load-plugin path))

(define [%unload-plugin path]
  (set! *plugins* (hash-remove *plugins* path)))

(define [unload-plugin name]
  (define path (plugin-path name))
  (%unload-plugin path))

(define [get-plugin name]
  (define path (plugin-path name))
  (when (not (hash-contains? *plugins* path))
    (%load-plugin path))
  (hash-get *plugins* path))

(define-syntax #%require-plugin
  (syntax-rules [only-in]
    [(_ plugin-name (only-in name ...))
     (begin
       (define name
         (or
           (%#maybe-module-get (plugin-module (get-plugin (quote plugin-name))) (quote name))
           (error "No symbol" (quote name) "in" (quote plugin-name))))
       ...)]))

(define [plugin-module plugin]
  (Plugin-builtin plugin))

(define [path-relevant? path]
  (define path (normalize-path path))
  (hash-contains? *plugins* path))

(define [apply-change change]
  (define path (car change))
  (define path (normalize-path path))
  (define val (cadr change))
  (match val
    ['reload (%load-plugin path)]
    [else void]))

(spawn
  (lambda ()
    (watch-loop (plugins-dir) #t path-relevant? apply-change)))