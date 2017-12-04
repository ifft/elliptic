#lang racket
(provide (all-defined-out))
;bitcoin's elliptic curve parameters
(define bc_E_a 0)
(define bc_E_b 7)
(define bc_p #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F)
(define bc_G #x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798)
(define bc_G_long #x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8)
(define bc_n #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141)

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
    (let ([carry (if (eq? carry 1) r0 (sqr carry))]
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

(define (factor-2 x)
  (let factor-2_aux ([x x] [q 0])
    (cond
      ((eq? (modulo x 2) 0) (factor-2_aux (quotient x 2) (add1 q)))
      (else (values q x))
      )
    )
  )

(define (find-non-residue p)
  (let*
    (
     [sub1p/2  (/ (sub1 p) 2)]
     [pow      (lambda (x) (pow-p x sub1p/2 p))]
     [residue? (lambda (x) (eq? (pow x) 1))]
     )
    (let find-non-residue_aux
      ([curr 2])
      (cond
        ((residue? curr) (find-non-residue_aux (add1 curr)))
        ; assert: x^((prime-1)/2) should be -1 otherwise
        ((not (= (pow curr) (sub1 p)))
         (error (format "ASSERT: (pow ~a) (~a)!= (sub1 ~a)" curr (pow curr) p))
         )
        (else curr)
        )
      )
    )
  )

; Tonelli-Shanks algorithm
(define (mod-sqr n p)
 (factor-2 n p)
)

(define (pow_bitcoin x n) (pow_exp x n (lambda (x) (modulo x bc_p))))
(define (pow-p x n p) (pow_exp x n (lambda (x) (modulo x p))))
(define (pow x n) (pow_exp x n identity))
; XXX for testing    
(define (pow-p2 x p) (pow-p x (/ (sub1 p) 2) p))
