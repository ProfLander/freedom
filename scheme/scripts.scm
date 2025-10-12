(provide get-script)

(require "core.scm")
(require "async.scm")

(require-builtin freedom/log)
(require-builtin freedom/fs)

(define *scripts* (hash))

(define [load-script name]
  (info! "Loading" name "...")
  (define path (string-append "scheme/" (symbol->string name) ".scm"))
  (define port (open-input-file path))
  (define src (read-port-to-string port))
  (define script (await (#%compile src path)))
  (set! *scripts* (hash-insert *scripts* name script)))

(define [unload-script name]
  (info! "Unloading" name "...")
  (set! *scripts* (hash-remove *scripts* name)))

(define [get-script name]
  (when (not (hash-try-get *scripts* name))
    (load-script name))
  (hash-get *scripts* name))

(define watcher (Watcher 0.1 #f))

(Watcher-watch watcher "scheme" '(RecursiveMode (NonRecursive)))

(define [DebouncedEvent-props de]
  (match de
    [(list 'DebouncedEvent props) props]
    [_ #f]))

(define [DebouncedEvent-event de]
  (cadr (assoc 'event (DebouncedEvent-props de))))

(define [Event-props ev]
  (match ev
    [(list 'Event props) props]
    [_ #f]))

(define [Event-kind ev]
  (cadr (assoc 'kind (Event-props ev))))

(define [Event-path ev]
  (caadr (assoc 'path (Event-props ev))))

(define [EventKind-kind kind]
  (match kind
    [(list EventKind kind) kind]))

(define [event-changes de]
  (define event (DebouncedEvent-event de))
  (define path (Event-path event))
  (define filename (file-name path))
  (define file-stem (trim-end-matches filename ".scm"))
  (define relevant (hash-contains? *scripts* file-stem))
  (if relevant
    (begin
      (define kind (Event-kind event))
      (define kind (EventKind-kind kind))
      (match kind
        [(list 'Modify _)
          (list file-stem 'reload)]
        [(list 'Remove _)
          (list file-stem 'unload)]
        [_ #f]))
    #f))

(define [apply-change change]
  (define key (car change))
  (define val (cadr change))
  (match val
    ['reload
      (begin
        (load-script key))]
    ['unload
      (begin
        (unload-script key))]))

(define [watch-loop]
  (while #t
    (define events (await (Watcher-events watcher)))
    (define changes (filter id (map event-changes events)))
    (for-each apply-change changes)))

(spawn watch-loop)