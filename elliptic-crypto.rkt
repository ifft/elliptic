#lang racket
(require "utility.rkt")
(require "modulo-arith.rkt")
(require "elliptic-arith.rkt")
(require sha)
(provide sign
         verify
         gen-priv-key
         )

(define (truncate message curve)
  (bitwise-and (sub1 (arithmetic-shift 1 (* 8 (elliptic-curve-bytelen curve)))) message)
  )

(define (sign message key curve)
 (let (
       [*p (lambda (P x) (scalar-mul P x curve))]
       [+n (lambda (a b) (modulo (+ a b) (elliptic-curve-n curve)))]
       [modulo-n (lambda (x) (modulo x (elliptic-curve-n curve)))]
      )
  (let* (
         [gen-priv-key (lambda () (gen-priv-key curve))]
         [message (truncate message curve)]
        )
   (let try-again ([k (gen-priv-key)])
    (let* ([P (*p (elliptic-curve-G curve) k)]
           [r (modulo-n (point-x P))]
          )
     (cond
      ((zero? r) (try-again (gen-priv-key)))
      (else 
       (let (
             [s (modulo-n (* (inverse-of k (elliptic-curve-n curve)) (+n message (* r key))))]
            )
        (cond
         ((zero? s) (try-again (gen-priv-key)))
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

(define (verify message r s pubkey curve)
 (let (
       [s^-1 (inverse-of s (elliptic-curve-n curve))]
       [message (truncate message curve)]
      )
  (let* ([u1 (modulo (* s^-1 message) (elliptic-curve-n curve))]
         [u2 (modulo (* s^-1 r) (elliptic-curve-n curve))]
         [Gu1 (scalar-mul (elliptic-curve-G curve) u1 curve)]
         [Hu2 (scalar-mul pubkey u2 curve)]
         [P (add-point Gu1 Hu2 curve)]
        )
   (equal? r (point-x P))
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

(define (gen-priv-key curve)
  (let loop ([result (random32byte)])
    (cond
      ((and (< result (elliptic-curve-n curve))
            (not (zero? result)))
       result)
      (else (loop (random32byte)))
      )
    )
  )

