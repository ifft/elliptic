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

; insert message bit-length to the last extradatalen bytes.
; last extradatalen bytes will be unconditionally overwritten
(define (insert-msglen msgbytes bit-len)
  (when (< (bytes-length msgbytes) extradatalen) (error 'insert-msglen "byte stream is too short"))
  (let ([bit-len (integer->integer-bytes bit-len extradatalen #f)])
    (bytes-append (subbytes msgbytes 0 (- (bytes-length msgbytes) 8)) bit-len
                  )
    )
  )

; pad message
; TODO implement it with ports
(define (padmessage msgbytes)
  (let* (
         [bit-len (* 8 (bytes-length msgbytes))]
         [msgbytes (addstopbit msgbytes)]
         )
    (insert-msglen (expand-message msgbytes) bit-len)
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

(define (ripemd160_f x y z)
 (bitwise-xor x y z)
 )

(define (ripemd160_g x y z)
  (bitwise-ior
    (bitwise-and x y)
    (bitwise-and
      (bitwise-not x)
      z
      )
    )
  )

(define (ripemd160_h x y z)
  (bitwise-xor
    (bitwise-ior
      x
      (bitwise-not y)
      )
    z
    )
  )

(define (ripemd160_i x y z)
  (bitwise-ior
    (bitwise-and x z)
    (bitwise-and
      y
      (bitwise-not z)
      )
    )
  )

(define (ripemd160_j x y z)
  (bitwise-xor
    x
    (bitwise-ior
      y
      (bitwise-not z)
      )
    )
  )

(define (crot-dword-left x n)
  (when (>= x (expt 2 32)) (error 'crot-dword-left "out of range"))
  (bitwise-bit-field
    (bitwise-ior
      (arithmetic-shift x n)
      (arithmetic-shift x (- (- 32 n)))
      )
    0
    32
    )
  )

;(define (F A B C D E X S))
