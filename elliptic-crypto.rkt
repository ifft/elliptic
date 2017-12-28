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
 (printf "msg: ~a~n" message)
 (printf "~a" (* 8 (elliptic-curve-bytelen curve)))
  (bitwise-and (sub1 (arithmetic-shift 1 (* 8 (elliptic-curve-bytelen curve)))) message)
  )

(define (sign message key curve)
 (with-helper-funcs curve
  (let* (
         [gen-priv-key (lambda () (gen-priv-key curve))]
         [message (truncate message curve)]
         )
    (printf "sign~nmessage: ~a~nkey: ~a~ncurve: ~a~n" message key curve)
    (let try-again ([k (gen-priv-key)])
      (let* ([P (* (elliptic-curve-G curve) k)]
             [r (point-x P)]
             )
        (cond
          ((zero? r) (try-again (gen-priv-key)))
          (else 
            (let ([s (* (inv k) (+ message (* r key)))])
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
  (with-helper-funcs curve
                     (let (
                           [s^-1 (inv s)]
                           [message (truncate message curve)]
                           )
                       (let* ([u1 (* s^-1 message)]
                              [u2 (* s^-1 r)]
                              [Gu1 (* (elliptic-curve-G curve) u1)]
                              [Hu2 (* pubkey u2)]
                              [P (+ Gu1 Hu2)]
                              )
                         (printf "message ~a~nr: ~a~ns: ~a~npubkey: ~a~nP: ~a~n" message r s pubkey P)
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

