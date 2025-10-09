(require-builtin freedom/async)
(require-builtin library)

(displayln "Print loop")
(print-hello)

(#%spawn 'print-loop)