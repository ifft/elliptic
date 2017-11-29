#lang racket
(provide (all-defined-out))
;bitcoin's elliptic curve parameters
(define bc_E_a 1)
(define bc_E_b 0)
(define bc_E_c 7)
(define bc_G 0)
(define bc_p 0)

;(define (mod_pow x n p)
;)
;(define (pow_exp r n)
;)

(define (pow_exp carry result r0 n)
  (let ([carry2 (if (eq? carry 1) r0 (sqr carry))]
        [>> (lambda (x) (arithmetic-shift x -1))])
    (printf "pow_exp carry: ~a result: ~a r0: ~a n: ~a~n" carry result r0 n)
    (cond
      ((zero? n) result)
      (else
        (if (eq? (modulo n 2) 0)
          (pow_exp carry2 result r0 (>> n))
          (pow_exp carry2 (* carry2 result) r0 (>> (sub1 n)))
          )
        )
      )
    )
  )

