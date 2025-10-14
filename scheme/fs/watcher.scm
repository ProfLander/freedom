(provide watch-loop stop-watcher)

(require-builtin freedom/log)

(#%require-plugin freedom_notify
  (only-in Watcher Watcher-watch Watcher-unwatch Watcher-events Watcher-stop))

(define watcher (Watcher 0.1 #f))

(define [watch path recursive]
  (Watcher-watch watcher path
    (list 'RecursiveMode
      (list (if recursive 'Recursive 'NonRecursive)))))

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

(define [event-changes relevant]
  (lambda (de)
    (define event (DebouncedEvent-event de))
    (define path (Event-path event))
    (if (relevant path)
      (begin
        (define kind (Event-kind event))
        (define kind (EventKind-kind kind))
        (match kind
          [(list 'Modify _)
            (list path 'reload)]
          [(list 'Remove _)
            (list path 'unload)]
          [else #f]))
      #f)))

(define [watch-loop path recursive relevant with]
  (info! "Watching" path "for changes")
  (watch path recursive)
  (define handle-changes (event-changes relevant))
  (while #t
    (define events (await (Watcher-events watcher)))
    (define changes (filter id (map handle-changes events)))
    (for-each with changes)))

(define [stop-watcher]
  (Watcher-stop watcher)
  (set! watcher void))