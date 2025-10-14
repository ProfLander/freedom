(#%require-plugin library (only-in print-hello))

(print-hello)

(async (spawn (get-script 'print-loop)))