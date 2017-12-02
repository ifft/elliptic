#lang racket
(provide (all-defined-out))
;bitcoin's elliptic curve parameters
(define bc_E_a 1)
(define bc_E_b 0)
(define bc_E_c 7)
(define bc_G 0)
(define bc_p 1)

;(define (mod_pow x n p)
;)
;(define (pow_exp r n)
;)

(define (pow_exp x n mod_prime)
  (let pow_exp_aux ([carry 1]
                    [result 1]
                    [r0 x]
                    [n n]
                    )
    (let ([carry (if (eq? carry 1) r0 (mod_prime (sqr carry)))]
          [>> (lambda (x) (arithmetic-shift x -1))])
      (printf "pow_exp carry: ~a result: ~a r0: ~a n: ~a~n" carry result r0 n)
      (cond
        ((zero? n) result)
        (else
          (if (eq? (modulo n 2) 0)
            (pow_exp_aux carry result r0 (>> n))
            (pow_exp_aux carry (mod_prime (* carry result)) r0 (>> (sub1 n)))
            )
          )
        )
      )
    )
  )

(define (pow_bitcoin x n) (pow_exp x n (lambda (x) (modulo x bc_p))))
(define (pow_p x n p) (pow_exp x n (lambda (x) (modulo x p))))
(define (pow x n) (pow_exp x n identity))
