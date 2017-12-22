#lang racket
(require "utility.rkt")
(require "modulo-arith.rkt")
(require "elliptic-arith.rkt")
(require sha)
(provide sign
         verify
         select-k)

(define (sign message key curve)
 (with-helper-funcs curve
  (let* ([select-k (lambda () (select-k curve))])
   (let try-again ([k (select-k)])
    (let* ([P (* (elliptic-curve-G curve) k)]
           [r (point-x P)]
          )
     (cond
      ((zero? r) (try-again (select-k)))
      (else 
       (let ([s (* (inv k) (+ message (* r key)))])
        (cond
         ((zero? s) (try-again (select-k)))
         (else (values r s))
        )
       )
      )
     )
    )
   )
  )
 )
 )

(define (verify message r signature curve)
 (with-helper-funcs curve
  (let ([s^-1 (inv signature)])
   (let* ([u1 (* s^-1 message)]
          [u2 (* s^-1 r)]
          [P (+ (* (elliptic-curve-G curve) u1) (* (elliptic-curve-G curve) message))]
         )
    (equal? r (point-x P))
   )
  )
 )
 )

(define (random16bit)
 (random #xFFFF)
)

(define (random32byte)
  (let loop ([i 16]
             [result 0]
             )
    (cond
      ((zero? i) result)
      (else
        (loop (sub1 i) (+ (arithmetic-shift result 16) (random16bit))
              )
        )
      )
    )
  )

(define (select-k curve)
  (let loop ([result (random32byte)])
    (cond
      ((and (< result (elliptic-curve-n curve))
            (not (zero? result)))
       result)
      (else (loop (random32byte)))
      )
    )
  )

