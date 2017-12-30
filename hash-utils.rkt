#lang racket
(provide 
 padmessage
 n-byte-int->number
)

(define blocklen 64)
(define extradatalen 8)

; expands message+extradatalen to the next bloclen boundary
(define (expand-message msgbytes)
  (let (
        [len (+ (bytes-length msgbytes) extradatalen)]
        [%   (lambda (x) (modulo x blocklen))]

        )
    (bytes-append msgbytes (make-bytes extradatalen 0) (make-bytes (% (- blocklen (% len))) 0))
    )
  )


; adds the stop-bit to the end of the message
(define (addstopbit msgbytes)
 (bytes-append msgbytes (bytes #x80))
  )

; insert message length to the last extradatalen bytes.
; last extradatalen bytes will be unconditionally overwritten
(define (insert-msglen msgbytes len)
  (when (< (bytes-length msgbytes) extradatalen) (error 'insert-msglen "byte stream is too short"))
  (let ([len (integer->integer-bytes len extradatalen #f)])
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
         )
    (insert-msglen (expand-message msgbytes) len)
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
