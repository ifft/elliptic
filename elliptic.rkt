#lang racket
(require "utility.rkt")
(require "modulo-arith.rkt")
(require "elliptic-arith.rkt")
(require sha)

(provide (all-defined-out))
;bitcoin's elliptic curve parameters
(define bc_E_a 0)
(define bc_E_b 7)
(define bc_n #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141)
(define bc_p #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F)
(define bc_G_long #x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8)

(define bc_G_x #x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798)
(define bc_G_y #x483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8)

(define debug (make-parameter #t))
(define devel (make-parameter #t))
(define dprintf
 (lambda x
   (when (debug) (apply printf x))
 )
)

(define bc_G (point bc_G_x bc_G_y))
(define bitcoin-curve (elliptic-curve bc_E_a bc_E_b bc_p bc_G bc_n))


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

(define (validate point curve)
  (let ([y12 (calc-y (point-x point) curve)])
    (and
      (or (equal? (point-y point) (car y12)) (equal? (point-y point) (cadr y12)))
      (< (point-x point) (elliptic-curve-p curve))
      (< (point-y point) (elliptic-curve-p curve))
      )
    )
  )

(define (sign message key curve)
  (let* ([mod (lambda (x) (modulo x (elliptic-curve-p curve)))]
         [mul (lambda (x n) (scalar-mul x n curve))]
         [inv (lambda (x) (inverse-of x (elliptic-curve-p curve)))]
         [select-k (lambda () (select-k curve))]
         [+ (lambda (a b) (mod (+ a b) (elliptic-curve-p curve)))]
         [* (lambda (a b) (mod (* a b) (elliptic-curve-p curve)))]
         )

    (let try-again ([k (select-k)])
      (let* ([P (mul (elliptic-curve-G curve) k)]
             [r (point-x P)]
             )
        (cond
          ((zero? r) (try-again (select-k)))
          (else 
            (let* ([k^-1 (inv k)]
                   [s (mod (* k^-1 (+ message (* r key))))])
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

(define (verify message r signature curve)
  (let* ([mod (lambda (x) (modulo x (elliptic-curve-p curve)))]
         [mul (lambda (x n) (scalar-mul x n curve))]
         [inv (lambda (x) (inverse-of x (elliptic-curve-p curve)))]
         [+ (lambda (a b) (mod (+ a b) (elliptic-curve-p curve)))]
         [+p (lambda (a b) (add-point a b curve))]
         [* (lambda (a b) (mod (* a b)))]
         )
    (let ([s^-1 (inv signature)])
      (let* ([u1 (* s^-1 message)]
             [u2 (* s^-1 r)]
             [P (+p (mul (elliptic-curve-G curve) u1) (mul (elliptic-curve-G curve) message))]
             )
       (equal? r (point-x P))
        )
      )
    )
  )
    ;Calculate the integer u1= s−1*z mod n
    ;Calculate the integer u2=s−1*r mod n
    ;Calculate the point P = u1*G + u2 * msg
    ;The signature is valid only if r=xP mod n

; to be removed. these are here only for testing
(when (devel) (random-seed 42))
(number->string (random32byte) 16)
