#lang racket
(require simple-qr
         file/sha1)
(define(qr-file f)
  (qr-code(bytes->hex-string (port->bytes(open-input-file f)))
          #:mode "A" #:error_level "L"))