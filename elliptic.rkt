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

(define (pow_exp x n)
  (let pow_exp_aux ([carry 1]
                    [result 1]
                    [r0 x]
                    [n n]
                    )
    (let ([carry (if (eq? carry 1) r0 (sqr carry))]
          [>> (lambda (x) (arithmetic-shift x -1))])
      (printf "pow_exp carry: ~a result: ~a r0: ~a n: ~a~n" carry result r0 n)
      (cond
        ((zero? n) result)
        (else
          (if (eq? (modulo n 2) 0)
            (pow_exp_aux carry result r0 (>> n))
            (pow_exp_aux carry (* carry result) r0 (>> (sub1 n)))
            )
          )
        )
      )
    )
  )
