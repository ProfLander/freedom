(provide #%load-plugin Plugin Plugin-builtin Plugin-library)

(require-builtin freedom/log)
(require-builtin freedom/tempfile)
(require-builtin freedom/loading)

(define interface-symbol (make-parameter "plugin"))

;; Extract plist from dylib interface
(define [plugin-props plugin]
  (match plugin
    [(list 'Plugin props) props]
    [_ #f]))

;; Extract property from plugin interface
(define [plugin-prop plugin prop]
  (cadr (assoc prop (plugin-props plugin))))

;; Extract init function from plugin interface
(define [plugin-init plugin]
  (plugin-prop plugin 'init))

;; Extract module constructor from plugin interface
(define [plugin-module plugin]
  (plugin-prop plugin 'module))

;; Handle to a loaded plugin
(struct Plugin (builtin library tempfile))

;; Load a plugin by path
(define [#%load-plugin path]
  (define tempfile (TempFile path))
  (info! "tempfile:" tempfile)

  ; Load the library
  (define library (Library (TempFile-path tempfile)))
  (info! "library:" library)

  ; Fetch its plugin interface
  (define plugin (Library-get library (interface-symbol)))
  (info! "plugin:" plugin)

  ; Initialize
  (define plugin-init (plugin-init plugin))
  (info! "calling plugin-init:" plugin-init)
  (plugin-init)

  ; Setup builtin module
  (define plugin-module (plugin-module plugin))
  (info! "extracting plugin module:" plugin-module)

  (define module (plugin-module))
  (info! "plugin-module:" module)
  
  ; Construct handle
  (Plugin module library tempfile))
