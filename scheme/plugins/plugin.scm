(provide #%load-plugin Plugin Plugin-builtin Plugin-tempfile)

(require "../fs/mod.scm")

(require-builtin freedom/log)
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
  (info! "Loading" path "...")

  (define temp-dir (temp-dir))
  (define tempfile (string-append temp-dir (unique-id) "-" (file-name path)))

  (info! "Copying" path "to tempfile at" tempfile "...");
  (copy-file! path tempfile)

  ; Load the library
  (define library (Library tempfile))

  ; Fetch its plugin interface
  (define plugin (Library-get library (interface-symbol)))

  ; Initialize
  (define plugin-init (plugin-init plugin))
  (plugin-init)

  ; Setup builtin module
  (define plugin-module (plugin-module plugin))

  (define module (plugin-module))
  
  ; Construct handle
  (Plugin module library tempfile))
