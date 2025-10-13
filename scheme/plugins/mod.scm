(provide
  plugins-dir plugins
  %load-plugin load-plugin
  %unload-plugin unload-plugin
  get-plugin
  #%require-plugin)

(require-builtin freedom/log)
(require-builtin freedom/loading)

(require "plugin.scm")

;; Search directory parameter
(define plugins-dir (make-parameter "target/debug/deps"))

(define *plugins* (hash))

(define [plugins]
  *plugins*)

;; Given a plugin name, return a file path
(define [plugin-path name]
  (string-append (plugins-dir) "/" (#%library-filename name)))

(define [try-cleanup old-plugin]
  (when old-plugin
    (define tempfile (Plugin-tempfile old-plugin))
    (info! "Dropping old plugin" old-plugin)
    (set! old-plugin void)
    (info! "Deleting tempfile" tempfile "...")
    (delete-file! tempfile)))

(define [%load-plugin path]
  (define plugin (#%load-plugin path))
  (define old-plugin (hash-try-get *plugins* path))
  (set! *plugins* (hash-insert *plugins* path plugin))
  (try-cleanup old-plugin)
  (info! "plugins:" *plugins*))

(define [load-plugin name]
  (define path (plugin-path name))
  (%load-plugin path))

(define [%unload-plugin path]
  (define old-plugin (hash-remove *plugins* path))
  (try-cleanup old-plugin))

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
