(#%require-plugin freedom_cpal (only-in Host-default Host-default-output-device Stream-build-output-sine Stream-play))

(define host (Host-default))
(info! "host:" host)

(define device (Host-default-output-device host))
(info! "device:" device)

(define stream (Stream-build-output-sine device 48000 440.0))
(info! "stream:" stream)

(info! "playing...")
(Stream-play stream)

(while #t
  (async (yield)))