#lang racket
(require rackunit "hash-utils.rkt")
(require rackunit "utility.rkt")
(require/expose "hash-utils.rkt"(expand-message insert-msglen addstopbit))

;;;;; RIPEMD 160 test vectors ;;;;;
(define ripemd160-test-vectors
  '(
    #("" "9c1185a5c5e9fc54612808977ee8f548b2258d31")
    #("a" "0bdc9d2d256b3ee9daae347be6f4dc835a467ffe")
    #("abc" "8eb208f7e05d987a9b044a8e98c6b087f15a0bfc")
    #("message digest"  "5d0689ef49d2fae572b881b123a85ffa21595f36")
    #("abcdefghijklmnopqrstuvwxyz" "f71c27109c692c1b56bbdceb5b9d2865b3708dbc")
    #("abcdbcdefghijklmnopq" "12a053384a9c0c88e405a06c27dcf49ada62eb2b")
    #("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789" "b0e20b6e3116640286ed3a87a5713079b21f5189")
    #("12345678901234567890123456789012345678901234567890123456789012345678901234567890" "9b752e45573d4b39f4dbd3323cab82bf63326bfb")
    )
  )
;TODO
;1 million times "a"	52783243c1697bdbe16d37f97f68f08325dc1528

;;;;; padhex ;;;;;
(check-equal? (padhex 258) "0102")
(check-equal? (padhex 255) "ff")
(check-equal? (padhex 65536) "010000")

;;;;; expand-message ;;;;;
; expand sequence shorter than 64 bytes
(let ([message #"message"])
  (check-equal?
    (expand-message message)
    (bytes-append message (make-bytes (- 64 (bytes-length message)) 0))
    )
  )

; expand exactly 56 byte long sequence
(let ([message (make-bytes 56 42)])
  (check-equal? (expand-message message)
                (bytes-append message (make-bytes 8 0))
                )
  )

; expand twice when message longer than 56 byte
(let ([message (make-bytes 57 42)])
  (check-equal? (expand-message message)
                (bytes-append message (make-bytes 7 0) (make-bytes 64 0))
                )
  )

; expand twice 64 byte long sequence
(let* ([message (make-bytes 64 42)]
       [expanded (expand-message message)]
       )
  (check-equal? (bytes-length expanded) 128)
  (check-equal?
    expanded
    (bytes-append message (make-bytes 64 0))
    )
  )

;;;;; insert-msglen ;;;;;
(check-exn exn:fail? (lambda () (insert-msglen #"1234567" 42)))

(check-equal? (insert-msglen #"12345678" 42)
 (integer->integer-bytes 42 8 #f #f)
              )

(check-equal? (insert-msglen (bytes-append #"123456789" (make-bytes 7 42)) 42)
              (bytes-append #"12345678" (integer->integer-bytes 42 8 #f #f)
                            )
              )

;;;;; addstopbit ;;;;;
(check-equal? (addstopbit #"message") (bytes-append #"message" (bytes #x80)))

;;;;; padmessage ;;;;;
; pad to 64, stopbit fits, len fits
(let* (
      [message #"message"]
      [len (bytes-length message)]
      )
  (check-equal?
    (padmessage message)
    (bytes-append message (bytes #x80) (make-bytes 48 0) (integer->integer-bytes len 8 #f #f))
    )
  )

; pad to 128, stopbit doesn't fit
(let* (
      [message (bytes-append #"message" (make-bytes (- 64 7) 0))]
      [len (bytes-length message)]
      )
  (check-equal?
    (padmessage message)
    (bytes-append message (bytes #x80) (make-bytes (- 64 1 8) 0) (integer->integer-bytes len 8 #f #f))
    )
  )

; pad to 128 stopbit fits, len fits
(let* (
      [message (bytes-append #"message" (make-bytes 64 42))]
      [len (bytes-length message)]
      )
  (check-equal?
    (padmessage message)
    (bytes-append message (bytes #x80) (make-bytes (- 64 16) 0) (integer->integer-bytes len 8 #f #f))
    )
  )

; pad to 128, stopbit fits, len does not fit
(let* (
      [message (bytes-append #"message" (make-bytes (- 64 7 1) 42))]
      [len (bytes-length message)]
      )
  (check-equal?
    (padmessage message)
    (bytes-append message (bytes #x80) (make-bytes 56 0) (integer->integer-bytes len 8 #f #f))
    )
  )

; pad to 192 stopbit fits, len does not fit
(let* (
      [message (bytes-append #"message" (make-bytes (- 128 7 1) 42))]
      [len (bytes-length message)]
      )
  (check-equal?
    (padmessage message)
    (bytes-append message (bytes #x80) (make-bytes 56) (integer->integer-bytes len 8 #f #f))
    )
  )

; some more testcases, inspired from RFC1321 MD5 docs: 3.1 "Append Padding Bits"
; case 1: padding + length exactly fit
(let*
  (
   [message (bytes-append (make-bytes (sub1 (/ 448 8)) 42))]
   [len (bytes-length message)]
   )
  (check-equal?
    (padmessage message)
    (bytes-append message (bytes #x80) (integer->integer-bytes len 8 #f #f))
    )
  )
; case 2: length doesn't fit
(let*
  (
   [message (bytes-append (make-bytes (/ 448 8) 42))]
   [len (bytes-length message)]
   )
  (check-equal?
    (padmessage message)
    (bytes-append message (bytes #x80) (make-bytes 7 0) (make-bytes 56 0) (integer->integer-bytes len 8 #f #f))
    )
  )

;;;;; n-byte-int->number ;;;;;
(check-equal?
  (n-byte-int->number 64 (bytes-append (bytes 1) (make-bytes 63 0)))
  (arithmetic-shift #x01 (* 63 8))
  )

(check-equal?
  (n-byte-int->number 64 (bytes-append (bytes 1 2) (make-bytes 61 0) (bytes 1)))
  (+
    (arithmetic-shift 1 (* 63 8))
    (arithmetic-shift 2 (* 62 8))
    1
    )
  )

(check-exn exn:fail? (lambda () (n-byte-int->number 5 (make-bytes 5 0))))


