#lang racket
(provide (all-defined-out))

(define (padhex num)
  (let ([raw (format "~x" num)])
    (if
      (not (zero? (modulo (string-length raw) 2)))
      (string-append "0" raw)
      raw
      )
    )
  )

(define (expand-message msgbytes)
  (bytes-append msgbytes
                (make-bytes 
                  (- 512 (modulo (bytes-length msgbytes) 512))
                  0
                  )
                )
  )

; adds the stop-bit to the end of the message
(define (addstopbit msgbytes)
 (bytes-append msgbytes (bytes #x80))
  )

; insert message length to the last 8 bytes.
; last 8 bytes will be unconditionally overwritten
(define (insert-msglen msgbytes len)
  (when (< (bytes-length msgbytes) 8) (error 'insert-msglen "byte stream is too short"))
  (let ([len (integer->integer-bytes len 8 #f #f)])
    (bytes-append (subbytes msgbytes 0 (- (bytes-length msgbytes) 8)) len
                  )
    )
  )

; pad message
(define (padmessage message)
  )
