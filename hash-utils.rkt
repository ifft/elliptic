#lang racket
(provide (all-defined-out))

(define blocklen 512)

; expands message to the next boundary
; when force-at-boundary-length? is #t, message will be expanded when length mod blocklen == 0
(define (expand-message msgbytes (force-at-boundary-length? #f))
  (let ([f
          (if force-at-boundary-length?
            identity
            (lambda (x) (modulo x blocklen))
            )
          ])
    (bytes-append msgbytes
                  (make-bytes 
                    (f (- blocklen (modulo (bytes-length msgbytes) blocklen)))
                    0
                    )
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
; TODO implement it with ports
(define (padmessage msgbytes)
  (let* (
         [len (bytes-length msgbytes)]
         [msgbytes (addstopbit msgbytes)]
         [extra-padding-needed? (< (- blocklen (modulo len blocklen)) 8)]
         )
    (insert-msglen
      (expand-message 
        (if extra-padding-needed? (expand-message msgbytes #t) msgbytes)
        )
      len
      )
    )
  )

(define (n-byte-int->number n msgbytes)
  (let-values ([(ret x)
                (let (
                      [by-8-bytes (* 8 8)]
                      [numsteps   (/ n 8)]
                      )
                 (unless (integer? numsteps) (error 'n-byte-int->number "n should be dividable by 8"))
                  (for/fold
                    (
                     [number 0]
                     [msg msgbytes]
                     )
                    ([i (in-range numsteps)])
                    (values (+ (arithmetic-shift number by-8-bytes)
                               (integer-bytes->integer msg #f #t 0 8)
                               )
                            (subbytes msg 8)
                            )
                    )
                  )
                ])
              ret
              )
  )
