(#%require-plugin library (only-in print-hello))

(print-hello)

(spawn (get-script 'print-loop))